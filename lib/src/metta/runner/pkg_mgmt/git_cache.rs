
//! Manages a local cache of modules cloned from git
//!
//! Currently all network activity is synchronous.  At some point it makes sense to move
//! to async in order to parallelize downloads, etc.  When that time comes I would like
//! to look at the `asyncgit` crate.  `https://crates.io/crates/asyncgit/0.26.0`
//!

use std::path::{Path, PathBuf};

use xxhash_rust::xxh3::xxh3_64;
use git2::{*, build::*};

use crate::metta::runner::environment::Environment;
use crate::metta::runner::modules::module_name_make_legal;

/// Indicates the desired behavior for updating the locally-cached repo
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UpdateMode {
    PullIfMissing,
    PullLatest,
    TryPullLatest,
}

pub struct CachedModule {
    _mod_name: String,
    url: String,
    branch: Option<String>,
    local_path: PathBuf,
}

impl CachedModule {
    /// Initializes a new CachedModule object
    ///
    /// * `cache_name` - A name to describe the cache.  For the default cache for URLs specified
    ///  from the [PkgInfo], the cache is named `git-modules`
    /// * `mod_name` - The catalog name of the module in the cache
    /// * `ident_str` - An ascii string that identifies the specific module among other verions
    ///  of the module.
    /// * `url` - The URL from which to fetch the module
    /// * `branch` - The branch to use, or default if None
    pub fn new(env: &Environment, cache_name: Option<&str>, mod_name: &str, ident_str: &str, url: &str, branch: Option<&str>) -> Result<Self, String> {
        let cache_name = cache_name.unwrap_or("git-modules");
        let working_dir = env.working_dir().ok_or_else(|| "Unable to clone git repository; no local working directory available".to_string())?;
        let branch_str = match &branch {
            Some(s) => s,
            None => ""
        };

        let unique_id = xxh3_64(format!("{}{}", ident_str, branch_str).as_bytes());
        let local_filename = format!("{mod_name}.{unique_id:16x}");
        let local_path = working_dir.join(cache_name).join(local_filename);

        std::fs::create_dir_all(&local_path).map_err(|e| e.to_string())?;

        Ok(Self {
            _mod_name: mod_name.to_string(),
            url: url.to_string(),
            branch: branch.map(|s| s.to_owned()),
            local_path,
        })
    }

    /// Updates a local cached repo with a remote repo, using `mode` behavior
    pub fn update(&self, mode: UpdateMode) -> Result<(), String> {
        match Repository::open(&self.local_path) {

            //We have an existing repo on disk
            Ok(repo) => {

                //Do a `git pull` to bring it up to date
                if mode == UpdateMode::PullLatest || mode == UpdateMode::TryPullLatest {
                    let mut remote = repo.find_remote("origin").map_err(|e| format!("Failed find 'origin' in git repo: {}, {}", self.url, e))?;
                    match remote.connect(Direction::Fetch) {
                        Ok(_) => {},
                        Err(e) => {
                            if mode == UpdateMode::PullLatest {
                                return Err(format!("Failed to connect to origin repo: {}, {}", self.url, e))
                            }
                        }
                    }

                    let branch = self.get_branch(&remote)?;
                    remote.fetch(&[&branch], None, None).map_err(|e| format!("Failed fetch updates to git repo: {}, {}", self.url, e))?;

                    let fetch_head = repo.find_reference("FETCH_HEAD").map_err(|e| e.to_string())?;
                    self.merge(&repo, &branch, &fetch_head).map_err(|e| format!("Failed to merge remote git repo: {}, {}", self.url, e))?;
                }
                Ok(())
            },
            Err(_) => {

                //We don't have a local repo, so clone it fresh
                let mut repo_builder = RepoBuilder::new();
                match &self.branch {
                    Some(branch) => {
                        repo_builder.branch(branch);
                    },
                    None => {}
                }
                match repo_builder.clone(&self.url, &self.local_path) {
                    Ok(_repo) => Ok(()),
                    Err(e) => Err(format!("Failed to clone git repo: {}, {}", self.url, e)),
                }
            },
        }
    }

    /// Internal method to get the branch name
    fn get_branch(&self, remote: &Remote) -> Result<String, String> {
        Ok(match &self.branch {
            Some(b) => b.to_owned(),
            None => remote.default_branch()
                .map_err(|e| format!("Failed to resolve default branch name for git repo: {}, {}", self.url, e))?
                .as_str().unwrap().to_string()
        })
    }

    /// Internal method to perform a merge.  Intended to approximate the `git merge` command-line behavior
    fn merge(&self, repo: &Repository, branch: &str, incomming_commit_ref: &Reference) -> Result<(), git2::Error> {
        let annotated_commit = repo.reference_to_annotated_commit(incomming_commit_ref)?;
        let analysis = repo.merge_analysis(&[&annotated_commit])?;

        if analysis.0.is_up_to_date() {
            return Ok(());
        } else if analysis.0.is_fast_forward() {
            // Fast-forwarding...
            let mut reference = repo.find_reference(branch)?;

            reference.set_target(annotated_commit.id(), "Fast-forward")?;
            repo.checkout_head(None)?;
        } else {
            //NOTE: the below code appears to work, but it isn't needed at the moment
            unreachable!();
            // // Normal merge...
            // let head_commit = repo.head()?.peel_to_commit()?;
            // let incomming_commit = Reference::peel_to_commit(incomming_commit_ref)?;
            // let mut idx = repo.merge_commits(&head_commit, &incomming_commit, None)?;

            // if idx.has_conflicts() {
            //     return Err(git2::Error::from_str("Merge conflicts detected"));
            // }

            // let result_tree = repo.find_tree(idx.write_tree_to(&repo)?)?;
            // let signature = repo.signature()?;
            // repo.commit(Some("HEAD"), &signature, &signature, "Merge commit", &result_tree, &[&head_commit, &incomming_commit])?;
        }

        Ok(())
    }

    /// Returns the file system path for the locally cloned repository
    pub fn local_path(&self) -> &Path {
        &self.local_path
    }
}

/// Extracts the module name from a `.git` URL
///
/// For example, `https://github.com/trueagi-io/hyperon-experimental.git` would be parsed
/// into "hyperon-experimental".  Returns None if the form of the URL isn't recognized
pub fn mod_name_from_url(url: &str) -> Option<String> {
    let without_ending = url.trim_end_matches(".git")
        .trim_end_matches("/");
    let without_mod_name = without_ending.trim_end_matches(|c| c != '/');
    let mod_name = &without_ending[without_mod_name.len()..];
    module_name_make_legal(mod_name)
}
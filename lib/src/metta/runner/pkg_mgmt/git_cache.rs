
//! Manages local caches of cloned git repos
//!
//! Currently all network activity is synchronous.  At some point it makes sense to move
//! to async in order to parallelize downloads, etc.  When that time comes I would like
//! to look at the `asyncgit` crate.  `https://crates.io/crates/asyncgit/0.26.0`
//!

use std::path::{Path, PathBuf};
use std::time::{SystemTime, Duration, UNIX_EPOCH};
use std::fs::{File, read_to_string};
use std::io::prelude::*;

use xxhash_rust::xxh3::xxh3_64;
use git2::{*, build::*};

use crate::metta::runner::environment::Environment;

const TIMESTAMP_FILENAME: &'static str = "_timestamp_";

/// Indicates the desired behavior for updating the locally-cached repo
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UpdateMode {
    /// Clones the repo if it doesn't exist, otherwise leaves it alone
    PullIfMissing,
    /// Pulls the latest from the remote repo.  Fails if the remote is unavailable
    PullLatest,
    /// Attempts to pull from the remote repo.  Continues with the existing repo if
    /// the remote is unavailable
    TryPullLatest,
    /// Attempts to pull from the remote repo is the local cache is older than the
    /// specified number of seconds.  Otherwise continues with the repo on the disk
    TryPullIfOlderThan(u64)
}

#[derive(Debug)]
pub struct CachedRepo {
    name: String,
    url: String,
    branch: Option<String>,
    local_path: PathBuf,
}

impl CachedRepo {
    /// Initializes a new CachedRepo object
    ///
    /// * `cache_name` - A name to describe the cache.  For the default cache for URLs specified
    ///  from the [PkgInfo], the cache is named `git-modules`
    /// * `name` - The name of this repo within in the cache.  Often equal to the catalog name of a module
    /// * `ident_str` - An ascii string that identifies the specific repo among other verions.
    ///  For example this could be a version, for a MeTTa module catalog cache.
    /// * `url` - The remote URL from which to fetch the repo
    /// * `branch` - The branch to use, or default if None
    pub fn new(env: &Environment, cache_name: Option<&str>, name: &str, ident_str: &str, url: &str, branch: Option<&str>) -> Result<Self, String> {
        let cache_name = cache_name.unwrap_or("git-modules");
        let caches_dir = env.caches_dir().ok_or_else(|| "Unable to clone git repository; no local \"caches\" directory available".to_string())?;

        let local_filename = if branch.is_some() || ident_str.len() > 0 {
            let branch_str = match &branch {
                Some(s) => s,
                None => ""
            };
            let unique_id = xxh3_64(format!("{}{}", ident_str, branch_str).as_bytes());
            format!("{name}.{unique_id:016x}")
        } else {
            name.to_string()
        };
        let local_path = caches_dir.join(cache_name).join(local_filename);

        std::fs::create_dir_all(&local_path).map_err(|e| e.to_string())?;

        Ok(Self {
            name: name.to_string(),
            url: url.to_string(),
            branch: branch.map(|s| s.to_owned()),
            local_path,
        })
    }

    /// Updates a local cached repo with a remote repo, using `mode` behavior.  Returns `true` if the
    /// repo was updated, and `false` if the repo was left unchanged
    pub fn update(&self, mode: UpdateMode) -> Result<bool, String> {
        match Repository::open(&self.local_path) {

            //We have an existing repo on disk
            Ok(repo) => {

                //Do a `git pull` to bring it up to date
                if mode == UpdateMode::PullLatest || mode == UpdateMode::TryPullLatest || self.check_timestamp(mode) {
                    let mut remote = repo.find_remote("origin").map_err(|e| format!("Failed find 'origin' in git repo: {}, {}", self.url, e))?;
                    match remote.connect(Direction::Fetch) {
                        Ok(_) => {},
                        Err(e) => {
                            if mode == UpdateMode::PullLatest {
                                return Err(format!("Failed to connect to origin repo: {}, {}", self.url, e))
                            } else {
                                // We couldn't connect, but the UpdateMode allows soft failure
                                return Ok(false)
                            }
                        }
                    }

                    let branch = self.get_branch(&remote)?;
                    remote.fetch(&[&branch], None, None).map_err(|e| format!("Failed fetch updates to git repo: {}, {}", self.url, e))?;

                    let fetch_head = repo.find_reference("FETCH_HEAD").map_err(|e| e.to_string())?;
                    self.merge(&repo, &branch, &fetch_head).map_err(|e| format!("Failed to merge remote git repo: {}, {}", self.url, e))?;
                    self.write_timestamp_file()?;
                    Ok(true)
                } else {
                    // The UpdateMode is set such that we don't need to check 
                    Ok(false)
                }
            },
            Err(_) => {

                //We don't have a local repo, so clone it fresh
                log::info!("cloning remote git repo: {}", self.name);
                let mut repo_builder = RepoBuilder::new();
                match &self.branch {
                    Some(branch) => {
                        repo_builder.branch(branch);
                    },
                    None => {}
                }
                match repo_builder.clone(&self.url, &self.local_path) {
                    Ok(_repo) => {
                        self.write_timestamp_file()?;
                        Ok(true)
                    },
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
            log::info!("fetching update from remote git repo: {}", self.name);
            let mut branch_ref = repo.find_reference(branch)?;
            branch_ref.set_target(annotated_commit.id(), "Fast-forward")?;
            repo.checkout_tree(&repo.find_object(annotated_commit.id(), Some(ObjectType::Commit))?, Some(CheckoutBuilder::default().force()))?;
            repo.set_head(branch_ref.name().unwrap())?;
        } else {
            panic!("Fatal Error: cached git repository at \"{}\" appears to be corrupt", self.local_path.display());
            //NOTE: the below code appears to work, but it isn't needed at the moment
            //
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

    /// Internal function to write the timestamp file, with the value of "now"
    fn write_timestamp_file(&self) -> Result<(), String> {
        let duration_since_epoch = SystemTime::now().duration_since(UNIX_EPOCH).unwrap();
        let file_path = self.local_path.join(TIMESTAMP_FILENAME);
        let mut file = File::create(&file_path).map_err(|e| format!("Error creating timestamp file at {}, {e}", file_path.display()))?;
        file.write_all(&format!("{:016x}", duration_since_epoch.as_secs()).into_bytes())
            .map_err(|e| format!("Error writing file: {}, {e}", file_path.display()))
    }

    /// Returns `true` if `mode == TryPullIfOlderThan`, and the timestamp file indicates
    /// that amount of time has elapsed.  Otherwise returns `false`
    fn check_timestamp(&self, mode: UpdateMode) -> bool {
        match mode {
            UpdateMode::TryPullIfOlderThan(secs) => {
                let file_path = self.local_path.join(TIMESTAMP_FILENAME);
                match read_to_string(&file_path) {
                    Ok(file_contents) => {
                        let val = u64::from_str_radix(&file_contents, 16).unwrap();
                        let timestamp_time = UNIX_EPOCH.checked_add(Duration::from_secs(val)).unwrap();
                        timestamp_time.elapsed().unwrap().as_secs() > secs
                    },
                    _ => true //No timestamp file means we should pull
                }
            },
            _ => false,
        }
    }

    /// Returns the file system path for the locally cloned repository
    pub fn local_path(&self) -> &Path {
        &self.local_path
    }
}

# Distributed AtomSpace setup

## Running DAS module tests locally

### Install `das-toolbox` in order to get `das-cli` (Debian based Linux users):
```
sudo apt -y update
sudo apt -y install wget

wget -O - http://45.77.4.33/apt-repo/setup.sh | sudo bash

sudo apt -y install das-toolbox

# >= 1.0.0
das-cli --version
```

You can also run `das-cli` from source, using python3 (other OS):
```
git clone https://github.com/singnet/das-toolbox.git

cd das-toolbox
git checkout tags/1.0.0

# Optional - Create a virtual env
python3 -m venv .venv
source .venv/bin/activate

pip install -e das-cli/
```

1. Set up `das-cli` configurations
```
das-cli config set
# Or
python3 das-cli/src/das_cli.py config set

# For all other prompts, you can just hit ENTER

>>>
? Choose the AtomDB backend:  MongoDB + Redis
Enter Redis port [40020]: 
Is it a Redis cluster? [y/N]: 
Enter MongoDB port [40021]: 
Enter MongoDB username [admin]: 
Enter MongoDB password [admin]: 
Is it a MongoDB cluster? [y/N]: 
Enter Jupyter Notebook port [40019]: 
Enter the Attention Broker port [40001]: 
Enter the Query Agent port [40002]: 
Enter the Link Creation Agent Server port [40003]: 
Enter the Inference Agent port [40004]: 
Enter the Evolution agent port [40005]: 
Enter the Context Broker port [40006]: 
Configuration file saved -> /Users/arturgontijo/.das

```

2. Start Databases
```
das-cli db start
# Or
python3 das-cli/src/das_cli.py db start

>>>
Starting Redis service...
Redis has started successfully on port 40020 at localhost, operating under the server user arturgontijo.
Starting MongoDB service...
MongoDB has started successfully on port 40021 at localhost, operating under the server user arturgontijo.
```

3. Load Databases with `animals.metta` content:
```
# We need the metta file's absolute path, moving it to /tmp for convinience
cp integration_tests/das/animals.metta /tmp

# Then
das-cli metta load /tmp/animals.metta
# Or
python3 das-cli/src/das_cli.py metta load /tmp/animals.metta

>>>
das-cli-mongodb-40021 is running on port 40021
das-cli-redis-40020 is running on port 40020
Loading metta file /tmp/animals.metta...
Connecting to Redis at localhost:40020
Connecting to MongoDB at localhost:40021
Done.
```

4. Start Attention Broker service
```
das-cli ab start
# Or
python3 das-cli/src/das_cli.py ab start

>>>
Starting Attention Broker service...
Attention Broker started on port 40001
```

5. Start Query Broker service
```
das-cli qa start
# Or
python3 das-cli/src/das_cli.py qa start

>>>
Starting Query Agent service...
Query Agent started on port 40002
```

6. Running DAS tests
```
# Run it passing a metta file (from repo's root)
cargo run -r --bin metta-repl integration_tests/das/test.metta

>>>
[()]
[()]
[ServiceAvailable: pattern_matching_query]
[()]
[()]
[()]
[()]
[()]

# Optional: use RUST_LOG=das=LEVEL to inspect workflow
RUST_LOG=das=debug ./target/release/metta-repl integration_tests/das/test.metta
```

7. Stop all services (removing their containers)
```
das-cli qa stop
das-cli ab stop
das-cli db stop

# Or
python3 das-cli/src/das_cli.py qa stop
python3 das-cli/src/das_cli.py ab stop
python3 das-cli/src/das_cli.py db stop
```

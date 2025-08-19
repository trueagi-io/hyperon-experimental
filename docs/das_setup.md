
## Running DAS module tests locally

### Install `das-toolbox` in order to get `das-cli` (linux users):
```
sudo apt -y update
sudo apt -y install build-essential wget

wget -O - http://45.77.4.33/apt-repo/setup.sh | sudo bash

sudo apt -y install das-toolbox

sudo das-cli update-version --version 0.5.0
das-cli --version
```

You can also run `das-cli` from source, using python3 (other OS):
```
git clone https://github.com/singnet/das-toolbox.git

cd das-toolbox
git checkout tags/0.5.0

# Optional - Create a virtual env
python3 -m venv .venv
source .venv/bin/activate

pip3 install -r das-cli/src/requirements.txt
```

1. Set up `das-cli` configurations
```
das-cli config set
# Or
python3 das-cli/src/das_cli.py config set

# NOTE: Be sure to set Attention Broker's port to 37007
# For all other prompts, you can just hit ENTER

>>>
Enter Redis port [40020]: 
Is it a Redis cluster? [y/N]: 
Enter MongoDB port [40021]: 
Enter MongoDB username [admin]: 
Enter MongoDB password [admin]: 
Is it a MongoDB cluster? [y/N]: 
Enter Jupyter Notebook port [40019]: 
Enter the Attention Broker port [40001]: 37007 <--- HERE
Enter the Query Agent port [40002]: 
Enter the Link Creation Agent Server port [40003]: 
Enter the Link Creation Agent buffer file [/tmp/requests_buffer.bin]: 
Enter the Link Creation Agent request interval (in seconds) [1]: 
Enter the Link Creation Agent thread count [1]: 
Enter the Link Creation Agent default timeout (in seconds) [10]: 
Do you want to save links to a Metta file? [Y/n]: 
Do you want to save links to the database? [Y/n]: 
Enter the Inference Agent port [40004]: 
Enter the Evolution agent port [40005]: 
Configuration file saved -> /home/gontijo/.das

```

2. Start Databases
```
das-cli db start
# Or
python3 das-cli/src/das_cli.py db start

>>>
Starting Redis service...
Redis has started successfully on port 29000 at localhost, operating under the server user arturgontijo.
Starting MongoDB service...
MongoDB has started successfully on port 28000 at localhost, operating under the server user arturgontijo.
```

3. Load Databases with `das_kb.metta` content:
```
# We need the metta file's absolute path, moving it to /tmp for convinience
cp lib/tests/das_kb.metta /tmp

# Then
das-cli metta load /tmp/das_kb.metta
# Or
python3 das-cli/src/das_cli.py metta load /tmp/das_kb.metta

>>>
das-cli-mongodb-28000 is running on port 28000
das-cli-redis-29000 is running on port 29000
Loading metta file /tmp/das_kb.metta...
Connecting to Redis at localhost:29000
Connecting to MongoDB at localhost:28000
Done.
```

4. Start Attention Broker service
```
das-cli ab start  
# Or
python3 das-cli/src/das_cli.py ab start

>>>
Starting Attention Broker service...
Attention Broker started on port 37007
```

5. Start Query Broker service
```
das-cli qa start  
# Or
python3 das-cli/src/das_cli.py qa start

>>>
# Chose a port range (using 52000:52999)
Enter port range (e.g., 3000:3010): 52000:52999
Starting Query Agent service...
Query Agent started on port 35700
```

6. Running DAS tests
```
# Be sure the REPL is build (from repo's root)
cargo b -r

# Run it passing a metta file
# NOTE: this run will freeze for ~20s as client tries to get all available services from peers
./target/release/metta-repl lib/tests/das.metta

>>>
[2025-08-19T12:41:03Z WARN  metta_bus_client::bus_node] BusNode::join_network(): Unable to get all services (1/4) from peer localhost:35700
[()]
[()]
[()]
[()]
[()]

# Optional: use RUST_LOG=das=LEVEL to inspect workflow
RUST_LOG=das=debug ./target/release/metta-repl lib/tests/das.metta
```

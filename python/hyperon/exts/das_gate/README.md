# How to setup DAS and use it with Metta

## How to setup DAS and use it localy in-ram

First you need to install [metta](https://github.com/trueagi-io/hyperon-experimental) and [hyperon-das](https://github.com/singnet/das-query-engine).
The easiest way to do that is to create a clean and isolated environment, activate it and run

```
pip install hyperon
pip install hyperon-das
```

You can check installation by creating a test.metta file with this content

```
!(import! &self das_gate)

!(bind! &das (new-das))

!(add-atom &das Test)
```
and running a command:

```
metta test.metta
```
If it's installed properly then you will get:

```
[]
[]
[]
```


## How to setup DAS and use it with your own end point

First you need to setup your own das server using instruction from [here](https://github.com/singnet/das-toolbox?tab=readme-ov-file#installation).
Then you have to install metta and hyperon-das using instructions above.
After that you can connect to your own endpoint using a command below (replace "Put your..." strings with your actual endpoint address):

```
!(bind! &das (new-remote-das "Put your IP here" "Put your port here"))
```



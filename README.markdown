# Overmind-Agents

## Usage

## Installation

- Install Roswell

```
sudo apt-get -y install git build-essential automake libcurl4-openssl-dev
git clone -b release https://github.com/roswell/roswell.git
cd roswell
sh bootstrap
./configure
make
sudo make install
ros setup
```

- Install these dependencies in `~/.roswell/local-projects/`:

```
git clone git@github.com:2old2randr/cl-dates.git
git clone git@github.com:lepisma/plotly-cl.git
git clone git@github.com:cgore/sigma.git
```

- Make copies of configuration files and edit them:

```
cp src/config.lisp.template src/config.lisp
cp src/db.lisp.template src/db.lisp
```

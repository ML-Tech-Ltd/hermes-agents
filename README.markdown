## Table of Contents

- [Hermes-Agents](#hermes-agents)
  - [Usage](#usage)
  - [Installation](#installation)
- [Extras](#extras)

# Hermes-Agents

## Usage

## Installation

- Update

```
sudo apt update
```

- Install these dependencies on Ubuntu based distros:

```
sudo apt-get install libmagic-dev
```

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
ros install sbcl
```

- Update ASDF

```
git clone https://gitlab.common-lisp.net/asdf/asdf.git
cd asdf
make
ros run
* (load "./tools/install-asdf.lisp")
```

- Install Ultralisp:

```
$ ros run
* (ql-dist:install-dist "http://dist.ultralisp.org/" :prompt nil)
* (quit)
```

- Install these dependencies in `~/.roswell/local-projects/`:

```
git clone https://github.com/2old2randr/cl-dates.git
git clone https://github.com/lepisma/plotly-cl.git
git clone https://github.com/cgore/sigma.git
git clone https://github.com/tsikov/clerk.git
git clone git@github.com:ml-tech-ltd/cl-rest-server.git
git clone git@github.com:ml-tech-ltd/genetic-algorithm.git
```

- Install all of Hermes Technologies. `Hermes Common` is private, but
  this dependency should not hinder the understanding of the other
  repositories, as it only defines simple utilities and parameters.

```
git clone git@github.com:ml-tech-ltd/hermes-common.git # Private Repo
git clone git@github.com:ml-tech-ltd/hermes-agents.git
git clone git@github.com:ml-tech-ltd/hermes-evolution.git
git clone git@github.com:ml-tech-ltd/hermes-input.git
git clone git@github.com:ml-tech-ltd/hermes-intuition.git
git clone git@github.com:ml-tech-ltd/hermes-perception.git
git clone git@github.com:ml-tech-ltd/hermes-research.git
```

- Install qlot:

```
ros install qlot
```

- Create configuration files and set the parameters to appropriate values:

```
cd hermes-common; cp src/db.lisp.template src/db.lisp
```

```
cd hermes-input; cp src/config.lisp.template src/config.lisp
```

- Install PostgreSQL

```
sudo apt install postgresql postgresql-contrib
sudo su - postgres -c "createuser hermes"
sudo su - postgres -c "createdb hermes"
sudo -u postgres psql
GRANT ALL PRIVILEGES ON DATABASE hermes TO hermes;
\password hermes
```

# Extras

- Emacs configuration

```
git clone git@github.com:amherag/home.git
cd home
./init.sh
```

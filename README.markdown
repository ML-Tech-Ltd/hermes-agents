# Hermes-Agents

## Usage

## Installation

- Install Roswell

```
sudo apt-get -y install git build-essential automake
libcurl4-openssl-dev git clone -b release
https://github.com/roswell/roswell.git cd roswell sh bootstrap
./configure make sudo make install ros setup
```

- Install these dependencies in `~/.roswell/local-projects/`:

```
git clone git@github.com:2old2randr/cl-dates.git
git clone git@github.com:lepisma/plotly-cl.git
git clone git@github.com:cgore/sigma.git
git clone git@github.com:tsikov/clerk.git
git clone git@github.com:ml-tech-ltd/cl-rest-server.git
```

- Install all of Hermes Technologies. `Hermes Common` is private, but
  this dependency should not hinder the understanding of the other
  repositories, as it only defines simple utilities and parameters.

```
git clone git@github.com:ml-tech-ltd/hermes-agents.git git clone
git@github.com:ml-tech-ltd/hermes-common.git # Private Repo git clone
git@github.com:ml-tech-ltd/hermes-evolution.git git clone
git@github.com:ml-tech-ltd/hermes-input.git git clone
git@github.com:ml-tech-ltd/hermes-intuition.git git clone
git@github.com:ml-tech-ltd/hermes-perception.git git clone
git@github.com:ml-tech-ltd/hermes-research.git
```

- Install qlot:

``` ros install qlot ```

- Install ultralisp:

```
$ ros run
* (ql-dist:install-dist "http://dist.ultralisp.org/" :prompt nil)
* (quit)
```

- Create configuration files and set the parameters to appropriate values:

```
cd hermes-common; cp src/db.lisp.template src/db.lisp
```

```
cd hermes-input; cp src/config.lisp.template src/config.lisp
```

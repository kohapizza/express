# README
## What is this repository for?

* *express* is the development environment for the Japanese automatic inference system [lightblue](https://github.com/DaisukeBekki/lightblue)
* Copyright owner: Koharu Saeki and Bekki Laboratory

## Installing express
### Prerequisite : Haskell Stack
In Linux:
```
$ wget -qO- https://get.haskellstack.org/ | sh
```
In Mac:
```
$ brew install haskell-stack
```
See https://docs/haskellstack.org/en/stable/README/#how-to-install for details.

### Download express
Do the following in the directory under which you'd like to install *express*.
```
$ git clone git@github.com:kohapizza/express.git
```
### Configuration and Installation
move to <express> and do the following:
```
$ cd <express>
$ stack build
```

## Running express
do the following:
```
$ stack run
```
and access localhost:3000 in your browser
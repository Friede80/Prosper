# Prosper

Prosper is a tool to automate investments on the peer-to-peer lending site Prosper.  Given a target portfolio distribution, this program will attempt to purchase available notes to keep your portfolio balanced.

## Installation

```Build
$ ghc Prosper.hs
```

## Usage

```console
Prosper.exe "path/to/configfile.txt
```

## Config file

The config file must be a JSON formated file with the following fields:

```
{
    "clientID":"XXXX",
    "clientSecret":"XXXX",
    "userID":"XXXX",
    "password":"XXXX",
    "distribution",[0.2, 0.3, 0.5, 0, 0, 0, 0]
}
```

Here `distribution` is an array where each filed indicates the percentage of the portfolio that should be invested in the corresponding note category:  [ "AA", "A", "B", "C", "D", "E", "HR" ]

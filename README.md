# stdout-progress

Simple command-line utility to measure progress of common long running tasks.

This was an educational project so code may be heavily borrowed from examples.

## Usage

Initial measurement:

```
$ stdout-progress [your command and args]
```

Subsequent runs:

```
$ stdout-progress -b [num bytes from previous] [your command]
```

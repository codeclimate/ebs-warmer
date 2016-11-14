Warm an EBS volume by reading all its bytes.

This is a small wrapper over the `dd` command found [here][docs].

[docs]: http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-initialize.html

The wrapper manages backgrounding the `dd` process and repeatedly sending
`SIGUSER1` to read its progress. The progress is then output in a nicer format
that includes estimated time remaining.

## Installation

There is a pre-built binary for 64 bit Linux:

```
curl https://s3.amazonaws.com/com.codeclimate.tools/ebs-warmer-0.1.0.0-linux-x86_64 > ebs-warmer
chmod +x ebs-warmer
sudo mv ebs-warmer /usr/local/bin
```

## Usage

```
Usage: ebs-warmer [-i|--interval SECONDS] DEVICE
  Warm an attached EBS volume by reading all its bytes

Available options:
  -h,--help                Show this help text
  -i,--interval SECONDS    Interval on which to print progress information
  DEVICE                   Block device to read, e.g. /dev/sda1
```

## Example

```
% sudo build/bin/ebs-warmer /dev/sda2
bytes=275.5MB total=238.2GB rate=91.8MB/s remaining=44m14s
bytes=631.0MB total=238.2GB rate=105.1MB/s remaining=38m34s
bytes=1.6GB total=238.2GB rate=192.6MB/s remaining=20m57s
```

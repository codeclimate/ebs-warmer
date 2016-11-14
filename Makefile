VERSION = $(shell sed '/^version: *\(.*\)$$/!d; s//\1/' ebs-warmer.cabal)

build/bin/ebs-warmer: app src
	docker run --rm \
	  --volume $(PWD):/src:ro \
	  --volume $(PWD)/build/stack:/root/.stack \
	  --volume $(PWD)/build/stack-work:/src/.stack-work \
	  --volume $(PWD)/build/bin:/opt/bin \
	  pbrisbin/static-hs

release: build/bin/ebs-warmer
	aws s3 cp --acl public-read build/bin/ebs-warmer \
	  s3://com.codeclimate.tools/ebs-warmer-$(VERSION)-linux-x86_64

.DEFAULT: build/bin/ebs-warmer

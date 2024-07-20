# This Dockerfile uses the statically-linked Slab binary from the GitHub
# release page, mostly to make sure it works on an "arbitrary" Linux
# distribution.
FROM debian:12.6-slim

RUN apt-get update \
  && apt-get install -y curl \
  && curl -sL https://github.com/hypered/slab/releases/download/v0.0.3.0-alpha/slab-v0.0.3.0-linux-x86-64-musl -o /bin/slab && chmod +x /bin/slab

RUN mkdir /tmp/src \
  && echo \
  "p Some non-ascii characters: çõé." \
  > /tmp/src/index.slab

RUN cd /tmp \
  && slab build src/

CMD cd /tmp/ \
  && slab build src/

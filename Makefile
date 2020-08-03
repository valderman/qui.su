.PHONY: all client server clean run docker docker-run push
DOCKER_TAG   ?= hootsman:latest
UPSTREAM_TAG ?= valderman/hootsman:latest
BUILDDIR      = ./_app

all: client server

run: all
	cd $(BUILDDIR) && ./hootsman-exe

push:
	docker build -t $(UPSTREAM_TAG) .
	docker push $(UPSTREAM_TAG)

docker: Dockerfile
	docker build -t $(DOCKER_TAG) .

docker-run: docker
	docker-compose up

client: $(BUILDDIR)
	cd client && npm install
	cd client && npm run build
	cp -rf client/build $(BUILDDIR)/static

server: $(BUILDDIR)
	cd server && \
	cabal v2-install \
		--install-method=copy \
		--overwrite-policy=always \
		--installdir ../$(BUILDDIR)

clean:
	rm -r $(BUILDDIR)
	rm -r client/build
	cd server && cabal v2-clean

$(BUILDDIR):
	mkdir -p $(BUILDDIR)

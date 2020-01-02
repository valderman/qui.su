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
	cd client && npm run build
	cp -r client/build $(BUILDDIR)/static

server: $(BUILDDIR)
	cd server && stack install --local-bin-path ../$(BUILDDIR)

clean:
	rm -r $(BUILDDIR)
	rm -r client/build
	cd server && stack clean

$(BUILDDIR):
	mkdir -p $(BUILDDIR)

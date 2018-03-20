sources := \
	src/jbuild \
	$(wildcard src/*.re)

executable := \
	_build/install/default/bin/pullies

build: $(executable)

clean:
	jbuilder clean

run: build
	$(executable) ls

install: build
	@jbuilder install

uninstall distclean:
	@jbuilder uninstall

distrib:
	[ -x $$(opam config var root)/plugins/opam-publish/repos/pullies ] || \
	  opam-publish repo add pullies mads-hartmann/pullies
	topkg tag
	topkg distrib

publish:
	topkg publish
	topkg opam pkg
	topkg opam submit

release: distrib publish

$(executable): $(sources)
	jbuilder build --dev


TEAM = eb8a5d4e-7839-4f5b-8355-584495cacb98
BUILD = build
IMAGE = ${BUILD}/image
RELEASE = ${BUILD}/icfp-${TEAM}.tar.gz
DIRECTORIES = ${BUILD} ${IMAGE}

## NOTE Added RELEASE to PHONY so it always builds, it is easier than risking getting a non-clean build. ##
.PHONY: default ${RELEASE}

default: ${RELEASE}

${RELEASE}: ${BUILD} ${IMAGE}
	./mafia build
	install -m 0755 ./dist/build/punter/punter ${IMAGE}/punter
	install -m 0755 ./install/install ${IMAGE}/install
	install -m 0644 ./install/README ${IMAGE}/README
	install -m 0644 ./install/PACKAGES ${IMAGE}/PACKAGES
	rsync -aH src ${IMAGE}/src
	rsync -aH test ${IMAGE}/test
	tar cvf ${RELEASE} -C ${IMAGE} .

${DIRECTORIES}:
	mkdir -p $@

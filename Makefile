#####################################################################################
# Copyright 2017 Normation SAS
#####################################################################################

.DEFAULT_GOAL := unlicensed

include makefiles/global-vars.mk

PUB_LIBS = 
PRIV_LIBS = plugins-common-private
LIBS= $(PUB_LIBS) $(PRIV_LIBS)

PLUGINS = api-authorizations auth-backends branding change-validation datasources helloworld node-external-reports scale-out-relay user-management vault glpi centreon patrowl
PLUGINS-LICENSED = $(addsuffix -licensed,$(PLUGINS))
ALL = $(LIBS) $(PLUGINS)

# all 
all: unlicensed

unlicensed: $(PUB_LIBS) $(PLUGINS)

licensed: $(LIBS) $(PLUGINS-LICENSED) 

$(LIBS):%:
	cd $@ && make

$(PLUGINS):%:
	cd $@ && make

$(PLUGINS-LICENSED):%-licensed:
	cd $* && make licensed

generate-all-pom: generate-pom
	for i in $(ALL); do cd $$i; $(MAKE) generate-pom; cd ..; done

doc-pages:
	mkdir -p doc/pages
	for i in $(PLUGINS); do cd $$i; [ -f README.adoc ] && sed '1,/\/\/ ====doc====/d' README.adoc >> ../doc/pages/$$i.adoc; cd ..; done

doc-assets:
	mkdir -p doc/assets
	for i in $(PLUGINS); do cd $$i; [ -d docs ] && cp -r docs/* ../doc/assets/; cd ..; done

doc: doc-assets doc-pages

clean: 
	rm -f pom.xml
	rm -rf doc
	for i in $(ALL); do cd $$i; $(MAKE) clean; cd ..; done

very-clean: clean 
	./makefiles/find_m2_repo.sh clean
	

.PHONY: $(LIBS) $(PLUGINS) doc

#
# make all : will build node-external-reports.rpkg.
# make licensed : will build a license limited version of the plugin
#

FILES = # add the file that need to be package here like $(NAME)/patrowl-schema.sql
SCRIPTS = postinst

include ../makefiles/common-scala-plugin.mk

target/$(NAME)/patrowl-schema.sql:
	cp ./src/main/resources/patrowl-schema.sql target/$(NAME)/


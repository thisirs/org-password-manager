.PHONY: documentation documentation/text documentation/deploy

project = org-password-manager

documentation: documentation/index.html documentation/$(project).txt

documentation/index.html: documentation/$(project).scrbl
	cd documentation && raco scribble --dest-name index -- $(project).scrbl

documentation/$(project).txt: documentation/$(project).scrbl
	cd documentation && raco scribble --text -- $(project).scrbl

documentation/deploy: documentation
	rsync -av --delete documentation/ leafac.com:leafac.com/websites/software/$(project)/

.PHONY: documentation documentation/text documentation/deploy documentation/clean clean

documentation: compiled-documentation/index.html

compiled-documentation/index.html: documentation/org-password-manager.scrbl
	raco scribble --dest compiled-documentation/ --dest-name index -- documentation/org-password-manager.scrbl

documentation/text: documentation/org-password-manager.txt

documentation/org-password-manager.txt: documentation/org-password-manager.scrbl
	raco scribble --dest documentation/ --text -- documentation/org-password-manager.scrbl

documentation/deploy: documentation
	rsync -av --delete compiled-documentation/ leafac.com:leafac.com/websites/software/org-password-manager/

documentation/clean:
	rm -rf compiled-documentation

clean: documentation/clean

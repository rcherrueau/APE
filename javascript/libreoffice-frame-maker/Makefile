ODP="diapo.odp"
NEW_ODP="diapo-frame.odp"

hack:
	http-server -d

pack:
	echo '<?xml version="1.0" encoding="UTF-8"?>' > content.xml
	xclip -o >> content.xml
	cp -f ${ODP} ${NEW_ODP}
	zip ${NEW_ODP} content.xml

unpack:
	unzip -o ${ODP} content.xml

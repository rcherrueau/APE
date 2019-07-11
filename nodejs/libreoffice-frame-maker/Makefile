ODP?="./pres.odp"
F_ODP?="./pres-framed.odp"
PDF?="./pres-framed.pdf"

all: import unpack frame pack

# Execute the NodeJS program
frame:
	node frameMaker.js

# Copy the original ODP into a new file for latter process (e.g.,
# `make unpack frame`)
import:
	cp -f "${ODP}" ${F_ODP}

# Extract the content.xml of the presentation for latter process
# (e.g., `make frame`)
unpack:
	unzip -o ${F_ODP} content.xml

# Put back the (modified) content.xml into the presentation
pack:
	zip ${F_ODP} content.xml

# Install NodeJS dependencies for this project
req:
	npm install jsdom@"^15.1.1"

# Export each page of a pdf into an jpg
export:
	rm -rf pres-images
	mkdir pres-images
	cd pres-images && convert -colorspace RGB -interlace none -density 300x300 -quality 100 ../${PDF} pres.jpg

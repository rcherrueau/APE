const fs = require('fs');
const jsdom = require("jsdom");
const { JSDOM } = jsdom;

let doc = null;

/** Do not apply frameMaker on these slides (start at 1) */
const excludeSlides = [/* 1,2,9,15,16,20,25,29,30,31,33,38,42,44,51,54 */];

/** Styles for the slides.
 *
 * Gives for a specific slide background color (e.g, #ffffff) the
 * frame stroke (e.g., #eeeeee) and back (e.g., #dddddd) colors, plus
 * the name of the theme (an alphanumeric word).
 *
 * > "#ffffff" : {                       // Slide background color
 * >     "frameStrokeColor": "#eeeeee",  // Frame stroke color
 * >     "frameBackColor":   "#dddddd",  // Frame background color
 * >     "name":             "myTheme",  // Theme name
 * > },
 */
const slideThemes = {
    "#ffffff": {  // White
        "name":             "white",
        "frameStrokeColor": "#fab7ae",
        "frameBackColor":   "#dddddd",
    },
    "#ffd7d7": {  // Pink
        "name":             "pink",
        "frameStrokeColor": "#416f6f",
        "frameBackColor":   "#ffffff",
    },
    "#608484": {  // Dark Green
        "name":             "dark-green",
        "frameStrokeColor": "#ffd7d7",
        "frameBackColor":   "#ffffff",
    },
    "#d2e9d2": {  // Light Green
        "name":             "light-green",
        "frameStrokeColor": "#b98f6c",
        "frameBackColor":   "#ffffff",
    },
    "default": {  // Default one -- mandatory
        "name":             "default",
        "frameStrokeColor": "#fab7ae",
        "frameBackColor":   "#dddddd",
    },
};

/** XML namespaces for office documents. */
const ns = {
    'style': 'urn:oasis:names:tc:opendocument:xmlns:style:1.0',
    'svg': 'urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0',
    'draw': 'urn:oasis:names:tc:opendocument:xmlns:drawing:1.0',
    'fo': 'urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0',
    'text': 'urn:oasis:names:tc:opendocument:xmlns:text:1.0',
};

/** Expand image region by a `factor`. */
function expand(factor, imgWidth, imgHeight, imgX, imgY) {
    let fWidth = imgWidth + factor;
    let fHeight = imgHeight + factor;
    let fX = imgX - (factor / 2.);
    let fY = imgY - (factor / 2.);

    return [fWidth, fHeight, fX, fY];
}

/** Inner frame expanding for a picture. */
function fIn(imgWidth, imgHeight, imgX, imgY) {
    return expand(0.25, imgWidth, imgHeight, imgX, imgY);
}

/** Outer frame expanding for a picture. */
function fOut(imgWidth, imgHeight, imgX, imgY) {
    let [fInWidth, fInHeight, fInX, fInY] =
        fIn(imgWidth, imgHeight, imgX, imgY);
    return expand(0.2, fInWidth, fInHeight, fInX, fInY);
}

/** Returns the theme of a slide. */
function findTheme(s) {
    let styleId = s.getAttribute("draw:style-name");
    let styleODP = doc.querySelector(
        "office\\:automatic-styles > "
            + `style\\:style[style:name="${styleId}"] > `
            + "style\\:drawing-page-properties");
    let backColor = styleODP.getAttribute("draw:fill-color");

    for (let [styleBackColor, styleOpts] of Object.entries(slideThemes)) {
        if (backColor == styleBackColor) { return styleOpts; }
    }

    // Default theme is the blank one
    return slideThemes['default'];
}

/** Push attributes to a node. */
function pushAttrs(node, attrObj) {
    for (let [attrName, value] of Object.entries(attrObj)) {
        let attrNS = attrName.split(':')[0];
        let attr = doc.createAttributeNS(ns[attrNS], attrName);
        attr.value = value;
        node.setAttributeNode(attr);
    }
}

/**
 * Make a style nodes for the inner and outer frame of a specific
 * theme and returns theme (in a list).
 *
 * The name is `${theme.name}-frame-in` four inner frame and
 * `${theme.name}-frame-out` for outer frame.
 *
 * <style:style xmlns:style="urn:oasis:names:tc:opendocument:xmlns:style:1.0"
 *         style:family="graphic"
 *         style:parent-style-name="standard">
 *     <style:graphic-properties
 *             xmlns:svg="urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0"
 *             svg:stroke-width="0.081cm"
 *             svg:stroke-color="#fab7ae"
 *             xmlns:draw="urn:oasis:names:tc:opendocument:xmlns:drawing:1.0"
 *             draw:marker-start-width="0.321cm"
 *             draw:marker-end-width="0.321cm"
 *             draw:fill="solid"
 *             draw:fill-color="#fffff"
 *             draw:textarea-horizontal-align="justify"
 *             draw:textarea-vertical-align="middle"
 *             draw:auto-grow-height="false"
 *             xmlns:fo="urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0"
 *             fo:min-height="5.766cm" fo:min-width="17.484cm"
 *             fo:padding-top="0.165cm" fo:padding-bottom="0.165cm"
 *             fo:padding-left="0.29cm"
 *             fo:padding-right="0.29cm"/>
 * </style:style>
 */
function mkStyleNode(theme) {
    let stylesNode = [];

    for (let [themeName,                 strokeWidth,       backFill]
         of [[`${theme.name}-frame-in`,  0.035,             "solid"],
             [`${theme.name}-frame-out`, 0.053,             "none"]]) {

        let styleNode = doc.createElementNS(ns['style'], "style:style");
        pushAttrs(styleNode, {
            'style:name': `${themeName}`,
            'style:family': 'graphic',
            'style:parent-style-name': 'standard',
        });

        let graphicNode = doc.createElementNS(ns['style'], "style:graphic-properties");
        pushAttrs(graphicNode, {
            'svg:stroke-width': `${strokeWidth}cm`,
            'svg:stroke-color': `${theme.frameStrokeColor}`,
            'draw:marker-start-width': "0.279cm",
            'draw:marker-end-width': "0.279cm",
            'draw:fill': `${backFill}`,
            'draw:fill-color': `${theme.frameBackColor}`,
            'draw:opacity': "25%",
            'draw:textarea-horizontal-align': "justify",
            'draw:textarea-vertical-align': "middle",
            'draw:auto-grow-height': "false",
            'fo:min-height': "8.431cm",
            'fo:min-width': "6.25cm",
            'fo:padding-top': "0.151cm",
            'fo:padding-bottom': "0.151cm",
            'fo:padding-left': "0.276cm",
            'fo:padding-right': "0.276cm"
        });

        styleNode.appendChild(graphicNode);
        stylesNode.push(styleNode);
    }

    return stylesNode;
}

/**
 * Make a frame node and returns it.
 *
 * <draw:custom-shape xmlns:draw="urn:oasis:names:tc:opendocument:xmlns:drawing:1.0"
 *         draw:style-name="gr40"
 *         draw:text-style-name="P12"
 *         draw:layer="layout"
 *         xmlns:svg="urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0"
 *         svg:width="9.507cm"
 *         svg:height="13.662cm"
 *         svg:x="14.505cm"
 *         svg:y="1.012cm">
 *     <text:p xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0"/>
 *     <draw:enhanced-geometry
 *             svg:viewBox="0 0 21600 21600"
 *             draw:type="rectangle"
 *             draw:enhanced-path="M 0 0 L 21600 0 21600 21600 0 21600 0 0 Z N"/>
 * </draw:custom-shape>
 */
function mkFrame(themeName, width, height, x, y) {
    // Make the draw:custom-shape
    let customShapeNode = doc.createElementNS(ns['draw'], "draw:custom-shape");
    pushAttrs(customShapeNode, {
        'draw:style-name': `${themeName}`,
        // 'draw:text-style-name': "P12"
        'draw:layer': "layout",
        'svg:width': `${width}cm`,
        'svg:height': `${height}cm`,
        'svg:x': `${x}cm`,
        'svg:y': `${y}cm`,
    });

    // Make the p
    let pNode = doc.createElementNS(ns['text'], 'text:p');
    customShapeNode.appendChild(pNode);

    // Make the draw:enhanced-geometry
    let enhancedGeoNode = doc.createElementNS(ns['draw'], "draw:enhanced-geometry");
    pushAttrs(enhancedGeoNode, {
        // 'svg:viewBox': "0 0 21600 21600",
        'draw:type': "rectangle",
        // 'draw:enhanced-path': "M 0 0 L 21600 0 21600 21600 0 21600 0 0 Z N",
    });

    customShapeNode.appendChild(enhancedGeoNode);

    return customShapeNode;
}

/**
 * Process one image.
 *
 * <draw:frame draw:style-name="gr1"
 *         draw:text-style-name="P1" draw:layer="layout"
 *         svg:width="7.004cm" svg:height="9.674cm"
 *         svg:x="11.172cm" svg:y="3.13cm">
 *     <draw:image xlink:href="Pictures/100000000000035A000004A13E781E0B3722F755.jpg"
 *             xlink:type="simple" xlink:show="embed"
 *             xlink:actuate="onLoad"
 *             loext:mime-type="image/jpeg">
 *         <text:p/>
 *     </draw:image>
 * </draw:frame>
 */
function processImage(img, theme) {
    // The `slice(0, -2)` remove the "cm" to retrieve the number in
    // e.g., `svg:width="7.004cm"`
    let imgWidth = parseFloat(img.getAttribute("svg:width").slice(0, -2));
    let imgHeight = parseFloat(img.getAttribute("svg:height").slice(0, -2));
    let imgX = parseFloat(img.getAttribute("svg:x").slice(0, -2));
    let imgY = parseFloat(img.getAttribute("svg:y").slice(0, -2));

    // Compute inner frame
    let [fInWidth, fInHeight, fInX, fInY] = fIn(imgWidth, imgHeight, imgX, imgY);
    let fInNode = mkFrame(`${theme.name}-frame-in`, fInWidth, fInHeight, fInX, fInY);

    // Compute outer frame
    let [fOutWidth, fOutHeight, fOutX, fOutY] = fOut(imgWidth, imgHeight, imgX, imgY);
    let fOutNode = mkFrame(`${theme.name}-frame-out`, fOutWidth, fOutHeight, fOutX, fOutY);

    // Put inner & outer frame in the DOM. The outer frame should
    // appear before the inner frame in the DOM since the order of
    // elements implements the odp depth. The first one in the list is
    // the deeper one in the odp page.
    img.parentNode.insertBefore(fInNode, img);
    img.parentNode.insertBefore(fOutNode, fInNode);
}

/**
 * Process one slide.
 *
 * <draw:page draw:name="page3" draw:style-name="dp3"
 *         draw:master-page-name="Default"
 *         presentation:presentation-page-layout-name="AL1T0">
 *     ...
 * </draw:page>
 */
function processSlide(s) {
    const page  = parseInt(s.getAttribute('draw:name').slice(4));
    const theme = findTheme(s);
    console.debug(page, theme.name);

    if (excludeSlides.includes(page)) return;

    // Get all images and process them.
    //
    // An `image` tag is enclosed into a `frame` tag and we have to
    // capture the outer one to get image info such as x, y, width,
    // height, ....
    //
    // Note: We only keep jpeg images (fortunately in my slides, all
    // photos are jpeg and other pictures are png, svg, ... which
    // makes filtering easy). FIXME: We also remove `draw:transform`
    // images because I don't know how to handle them right now.
    let imagesODP = (Array.from(s.querySelectorAll('draw\\:frame > draw\\:image[loext:mime-type="image/jpeg"]'))
                     .map(i => i.parentNode)
                     // TODO: remove this after handling of `draw:transform`
                     .filter(i => !i.hasAttribute("draw:transform")));
    console.debug('#images: ', imagesODP.length);
    imagesODP.forEach((i) => processImage(i, theme));
}

function processPresentation() {
    // Make and add style nodes for inner and outer frames.
    const stylesODP = doc.querySelector("office\\:automatic-styles");
    for (let [_, themeOpts] of Object.entries(slideThemes)) {
        styleNodes = mkStyleNode(themeOpts);
        styleNodes.forEach((n) => stylesODP.appendChild(n));
    }

    // Get the slides and process them
    let slidesODP = doc.querySelectorAll("office\\:presentation > draw\\:page");
    // console.debug(slidesODP);
    slidesODP.forEach(processSlide);
}

// MAIN
JSDOM.fromFile("content.xml", {
    contentType: "text/xml; charset=utf-8",
    resources: "usable", })
    .then((dom) => {
        // Get the presentation
        doc = dom.window.document;
        // console.debug(doc);

        // Process the document
        console.debug('Start processing...');
        processPresentation();

        // Render out the updated DOM
        fs.writeFile('content.xml', dom.serialize(), (err) => {
            if (err) throw err;
            console.debug('... Finished!');
        });
    })
    .catch((e) => console.error(e));

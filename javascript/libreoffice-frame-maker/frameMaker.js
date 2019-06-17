const xhr = new XMLHttpRequest();
const view = document.getElementById("main");
let doc = null;
let ii = null;

/** Styles for the slides. */
const slideStyles = {
    "white": {
        "slideBackColor":   "#ffffff",
        "frameStrokeColor": "#fab7ae",
        "frameBackColor":   "#eeeeee",
    },
    "pink": {
        "slideBackColor":   "#ffd7d7",
        "frameStrokeColor": "#416f6f",
        "frameBackColor":   "#ffffff",
    },
    "darkGreen": {
        "slideBackColor":   "#608484",
        "frameStrokeColor": "#ffd7d7",
        "frameBackColor":   "#ffffff",
    },
    "green": {
        "slideBackColor":   "#d2e9d2",
        "frameStrokeColor": "#b98f6c",
        "frameBackColor":   "#ffffff",
    },
    "default": {
        "slideBackColor":   "#ffffff",
        "frameStrokeColor": "#fab7ae",
        "frameBackColor":   "#eeeeee",
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

/** Inner frame grows for a picture. */
function fIn(imgWidth, imgHeight, imgX, imgY) {
    const fIn  = 0.25;

    let fInWidth = imgWidth + fIn;
    let fInHeight = imgHeight + fIn;
    let fInX = imgX - (fIn / 2.);
    let fInY = imgY - (fIn / 2.);

    return [fInWidth, fInHeight, fInX, fInY];
}

/** Outer frame grows for a picture. */
function fOut(imgWidth, imgHeight, imgX, imgY) {
    const fOut  = 0.2;

    let [fInWidth, fInHeight, fInX, fInY] =
        fIn(imgWidth, imgHeight, imgX, imgY);

    let fOutWidth = fInWidth + fOut;
    let fOutHeight = fInHeight + fOut;
    let fOutX = fInX - (fOut / 2.);
    let fOutY = fInY - (fOut / 2.);

    return [fOutWidth, fOutHeight, fOutX, fOutY];
}

/** Returns the style name of a slide page. */
function findStyleName(page) {
    if (page == 1 || (page >= 30 && page <= 43)) {
        return "white";
    } else if ((page >= 2 && page <= 11) || (page >= 16 && page <= 25)) {
        return "pink";
    } else if ((page >= 12 && page <= 15) || (page >= 26 && page <= 29)) {
        return "darkGreen";
    } else if (page >= 44 && page <= 55) {
        return "green";
    } else {
        return "default";
    }
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
 * Make the style Node for inner and outer frame and returns them.
 *
 * The name is `${name}-frame-in` four inner frame and
 * `${name}-frame-out` for outer frame.
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
function mkStyleNode(name, style) {
    let stylesNode = [];

    for (let [styleName, strokeWidth, fill]
         of [[`${name}-frame-in`, "0.035cm", "solid"],
             [`${name}-frame-out`, "0.053cm", "none"]]) {

        let styleNode = doc.createElementNS(ns['style'], "style:style");
        pushAttrs(styleNode, {
            'style:name': `${styleName}`,
            'style:family': 'graphic',
            'style:parent-style-name': 'standard',
        });

        let graphicNode = doc.createElementNS(ns['style'], "style:graphic-properties");
        pushAttrs(graphicNode, {
            'svg:stroke-width': `${strokeWidth}`,
            'svg:stroke-color': `${style.frameStrokeColor}`,
            'draw:marker-start-width': "0.279cm",
            'draw:marker-end-width': "0.279cm",
            'draw:fill': `${fill}`,
            'draw:fill-color': `${style.frameBackColor}`,
            'draw:opacity': "40%",
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
function mkFrame(styleName, width, height, x, y) {
    // Make the draw:custom-shape
    let customShapeNode = doc.createElementNS(ns['draw'], "draw:custom-shape");
    pushAttrs(customShapeNode, {
        'draw:style-name': `${styleName}`,
        'draw:text-style-name': "P12",
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
        'svg:viewBox': "0 0 21600 21600",
        'draw:type': "rectangle",
        'draw:enhanced-path': "M 0 0 L 21600 0 21600 21600 0 21600 0 0 Z N",
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
function processImage(img, styleName) {
    // The `slice(0, -2)` remove the "cm" to retrieve the number in
    // e.g., `svg:width="7.004cm"`
    let imgWidth = parseFloat(img.getAttribute("svg:width").slice(0, -2));
    let imgHeight = parseFloat(img.getAttribute("svg:height").slice(0, -2));
    let imgX = parseFloat(img.getAttribute("svg:x").slice(0, -2));
    let imgY = parseFloat(img.getAttribute("svg:y").slice(0, -2));

    // Compute inner frame
    let [fInWidth, fInHeight, fInX, fInY] = fIn(imgWidth, imgHeight, imgX, imgY);
    let fInNode = mkFrame(`${styleName}-frame-in`, fInWidth, fInHeight, fInX, fInY);

    // Compute outer frame
    let [fOutWidth, fOutHeight, fOutX, fOutY] = fOut(imgWidth, imgHeight, imgX, imgY);
    let fOutNode = mkFrame(`${styleName}-frame-out`, fOutWidth, fOutHeight, fOutX, fOutY);

    // Put inner, outer frame in the DOM. The outer frame should
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
    // Find slide style
    const page = parseInt(s.getAttribute("draw:name").slice(4));
    const styleName = findStyleName(page);
    console.debug(page, styleName);

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
    let imagesODP = (Array.from(s.querySelectorAll("frame > image"))
                     .filter(i => i.getAttribute("loext:mime-type") == "image/jpeg")
                     .map(i => i.parentNode)
                     // TODO: remove this after handling of `draw:transform`
                     .filter(i => !i.hasAttribute("draw:transform")));
    imagesODP.forEach((i) => processImage(i, styleName));
}

/** Render out the updated DOM. */
function render() {
    view.textContent = doc.documentElement.outerHTML;
    let range = document.createRange();
    range.selectNode(view);
    window.getSelection().addRange(range);
    // window.prompt("Copy to clipboard: Ctrl+C, Enter", doc.documentElement.outerHTML);
}

function main() {
    // Get the doc
    doc = xhr.responseXML;

    // Put style nodes for inner and outer frames.
    const stylesODP = doc.querySelector("automatic-styles");
    for (let [styleName, styleOpts] of Object.entries(slideStyles)) {
        styleNodes = mkStyleNode(styleName, styleOpts);
        styleNodes.forEach((n) => stylesODP.appendChild(n));
    }

    // Get the slides and process them
    let slidesODP = doc.querySelectorAll("presentation > page");
    slidesODP.forEach(processSlide);

    // Display the modified doc
    render();
}

xhr.onerror = () => {console.err("Error while getting XML.");};
xhr.onload  = main;
xhr.open("GET", "content.xml", false);
xhr.send(null);

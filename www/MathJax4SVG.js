//
//	$URL: svn+ssh://concyclic@svn.code.sf.net/p/mathjax4svg/code/sources/MathJax4SVG.js $
//	$Id: MathJax4SVG.js 8 2014-06-06 20:29:42Z concyclic $
//
//	Copyright © 2014 A. Shvetz
//
const NS_XHTML="http://www.w3.org/1999/xhtml";
const NS_MATHML="http://www.w3.org/1998/Math/MathML";
const NS_SVG="http://www.w3.org/2000/svg";
const NS_MATHJAX4SVG="http://mathjax4svg.sf.net";
const MATHJAX_URL="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=MML_HTMLorMML";
const SQRT2=Math.sqrt(2);
const DEFAULT_LABEL_OFFSET=".3em";

function isSVGDocument(document)
{
	if(document.defaultView.SVGDocument==undefined)
		return document.documentElement.namespaceURI==NS_SVG;
	else
		return document instanceof document.defaultView.SVGDocument;
}

function isHTMLDocument(document)
{
	return document instanceof HTMLDocument||document instanceof Document;
}

function processDocument()
{
	if(isSVGDocument(document)) processSVGDocument(document);
	else if(isHTMLDocument(document)) processHTMLDocument(document);
}

if(Document.prototype.getElementsByTagNameNS==undefined)
Document.prototype.getElementsByTagNameNS=function(namespaceURI, tagName)
	{
		var nl=this.getElementsByTagName(tagName);
		return nl;
	};

if(Number.prototype.parseFloat==undefined)
Number.prototype.parseFloat=function(x)
	{
		// Don't work in Konqueror
		return new Float(x);
	};

function processSVGDocument(document)
{
	var nlForeignObjects=document.getElementsByTagNameNS(NS_SVG, "foreignObject");
	for(var i=0; i<nlForeignObjects.length; i++)
		processForeignObject(nlForeignObjects[i]);
}

function processHTMLDocument(document)
{
	// Process objects
	var nlObjects=document.getElementsByTagNameNS(document.documentElement.namespaceURI, "object");
	for(var i=0; i<nlObjects.length; i++)
	{
		if(nlObjects[i].type!="image/svg+xml") return;
		processSVGDocument(nlObjects[i].contentDocument||nlObjects[i].getSVGDocument());
	}

	// Process immersed SVG
	var nlImmersed=document.getElementsByTagNameNS(NS_SVG, "svg");
	for(var i=0; i<nlImmersed.length; i++)
		processSVGDocument(nlImmersed[i]);
}

function processForeignObject(elForeignObject)
{
	//var elIframe=document.createElementNS(NS_XHTML, "object");
	var elIframe=elForeignObject.ownerDocument.createElementNS(NS_XHTML, "object");
	elIframe.type="application/xhtml+xml";
	elIframe.data="about:blank";
	elIframe.style.border="none";
	//var elIframe=document.createElementNS(NS_XHTML, "iframe");
	//elIframe.style.border="none";

	var elMath=elForeignObject.firstElementChild;
	if(elMath.nodeName!="math"||elMath.namespaceURI!=NS_MATHML) return;
	elForeignObject.replaceChild(elIframe, elMath);

	var setupMathJax=function()
		{
			var docIframeContentDocument=elIframe.contentDocument;//||elIframe.contentWindow.document;
			if(docIframeContentDocument==null)
			{
				console.log("DELAY");
				setTimeout(setupMathJax, 100);
				return;
			}
			var innerWindow=docIframeContentDocument.defaultView;

			innerWindow.setTimeout(function()
				{
					if(docIframeContentDocument.head.firstElementChild) return;
					var elMathJaxScript=docIframeContentDocument.createElementNS(NS_XHTML, "script");
					elMathJaxScript.src=MATHJAX_URL;
					elMathJaxScript.type="application/ecmascript";
					docIframeContentDocument.head.appendChild(elMathJaxScript);

				}, 0);
			
			if(innerWindow.MathJax==undefined)
			{
				innerWindow.setTimeout(setupMathJax, 100);
				return;				
			}
			docIframeContentDocument.body.style.backgroundColor="none";
			docIframeContentDocument.body.style.margin="0 0 0 0";
			docIframeContentDocument.body.appendChild(docIframeContentDocument.adoptNode(elMath));
			
			innerWindow.MathJax.Hub.Config({
					"HTML-CSS": {showMathMenu: false}
				});
			innerWindow.MathJax.Hub.queue.Push(function()
				{
					var elMathSpan=docIframeContentDocument.getElementsByClassName("math")[0];
					var rectMathSpan=elMathSpan.getBoundingClientRect();
					if(elForeignObject.getAttribute("width")==null)
						elForeignObject.setAttribute("width", rectMathSpan.width);
					if(elForeignObject.getAttribute("height")==null)
						elForeignObject.setAttribute("height", rectMathSpan.height);

					// label placement
					var labelPlacement=elForeignObject.getAttributeNS(NS_MATHJAX4SVG, "labelPlacement");
					if(labelPlacement)
					{
						//var labelX=Number.parseFloat(elForeignObject.getAttributeNS(NS_MATHJAX4SVG, "labelX"))||0;
						var labelX=parseFloat(elForeignObject.getAttributeNS(NS_MATHJAX4SVG, "labelX"))||0;
						//var labelY=Number.parseFloat(elForeignObject.getAttributeNS(NS_MATHJAX4SVG, "labelY"))||0;
						var labelY=parseFloat(elForeignObject.getAttributeNS(NS_MATHJAX4SVG, "labelY"))||0;
						var labelLeft=labelX, labelTop=labelY;
						

						var dummy=elForeignObject.ownerDocument.createElementNS(NS_SVG, "defs");
						elForeignObject.appendChild(dummy);
						dummy.style.width=DEFAULT_LABEL_OFFSET;
						//var labelOffset=Number.parseFloat
						var labelOffset=parseFloat
							(
								elForeignObject.getAttributeNS(NS_MATHJAX4SVG, "labelOffset")
									||dummy.ownerDocument.defaultView.getComputedStyle(dummy).width.replace(/px$/, "")
							);
						elForeignObject.removeChild(dummy);
						//console.log(innerWindow.getComputedStyle(docIframeContentDocument.body).fontSize);

						switch(labelPlacement)
						{
							case "right":
								labelTop-=rectMathSpan.height/2;
								labelLeft+=labelOffset;
								break;
							case "upperRight":
								labelLeft+=labelOffset/SQRT2;
								labelTop-=rectMathSpan.height+labelOffset/SQRT2;
								break;
							case "top":
								labelLeft-=rectMathSpan.width/2;
								labelTop-=rectMathSpan.height+labelOffset;
								break;
							case "upperLeft":
								labelLeft-=rectMathSpan.width+labelOffset/SQRT2;
								labelTop-=rectMathSpan.height+labelOffset/SQRT2;
								break;
							case "left":
								labelLeft-=rectMathSpan.width+labelOffset;
								labelTop-=rectMathSpan.height/2;
								break;
							case "lowerLeft":
								labelLeft-=rectMathSpan.width+labelOffset/SQRT2;
								labelTop+=labelOffset/SQRT2;
								break;
							case "bottom":
								labelLeft-=rectMathSpan.width/2;
								labelTop+=labelOffset;
								break;
							case "lowerRight":
								labelLeft+=labelOffset/SQRT2;
								labelTop+=labelOffset/SQRT2;
								break;
							case "center":
							default:
								labelLeft-=rectMathSpan.width/2;
								labelTop-=rectMathSpan.height/2;
								break;
						}
						elForeignObject.setAttribute("x", labelLeft);
						elForeignObject.setAttribute("y", labelTop);
					}
				});
		};
	setupMathJax();
}

addEventListener("load", processDocument, false);

/*
 * Copyright © 2026 Genworks International
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.  Distributed WITHOUT
 * ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.
 */

var puiResizeTimer;
var puiResizeDont = false;

function puiResize (rp = 'nil')
{
    if (puiResizeDont) return;
    else
    {
        clearTimeout(puiResizeTimer);
        
        puiResizeTimer = setTimeout(() => {
            gdlAjax(null, 'args=' + encode64('(:|iid| '+ doublequote + gdliid + doublequote + ' :|bashee| (:%rp% ' + rp + ') :|function| :set-slot! :|arguments| (:viewport-dimensions (:width ' + (document.getElementById('viewport').getBoundingClientRect().width) +  ' :length ' + (document.getElementById('viewport').getBoundingClientRect().height) + ')))'), true );}, 250);
    }
}



function gdlUpdate (request) {

 if (request.readyState == 1)
   if (document.getElementById('gdlStatus'))
    document.getElementById('gdlStatus').innerHTML = 'Working...';

 if (request.readyState == 2)
   if (document.getElementById('gdlStatus'))
    document.getElementById('gdlStatus').innerHTML = 'Got Error!';

 if (request.readyState == 3)
   if (document.getElementById('gdlStatus'))
     document.getElementById('gdlStatus').innerHTML = 'Almost There...';

 if ((request.readyState == 4) && (request.status == 200))
    {

	var root = request.responseXML.documentElement;
	var children = root.childNodes;
	var myelem;
	var codes;

        if (children)
	for (i=0; i< children.length; i++)
	{
	    var child=children[i];
	    var myid = null;
	    if (child.getElementsByTagName('replaceId')[0].firstChild != null)
            {
		myid = child.getElementsByTagName('replaceId')[0].firstChild.data
            }

	    var newHTML = null;
	    
	    if (child.getElementsByTagName('newHTML')[0].firstChild != null)
            {newHTML = child.getElementsByTagName('newHTML')[0].firstChild.nodeValue}

	    var jsToEval = null;

	    if (child.getElementsByTagName('jsToEval')[0].firstChild != null)
	    {jsToEval = child.getElementsByTagName('jsToEval')[0].firstChild.nodeValue}

	    if (myid && (newHTML != null))
            {
		var myelem = document.getElementById(myid);
                
		if (myelem) myelem.innerHTML = newHTML;

		if (jsToEval && (jsToEval == 'parseme'))
		{
		    if (myelem) codes = myelem.getElementsByTagName("script");
                    else codes = null;

                    if (codes)
		        for (var j=0;j<codes.length;j++)
		    {
			var text = codes[j].text;
			if (text) eval(text);
		    }}
		
            }
	    
	    if (jsToEval && (jsToEval != 'parseme') && (jsToEval != ''))
	    eval(jsToEval);
	}


	if (document.getElementById('gdlStatus'))
	{
	    document.getElementById('gdlStatus').innerHTML = 'Done.';
	}
    }}


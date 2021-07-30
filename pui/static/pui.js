

var resizeTimer;

function puiResize(rp = 'nil')
{

    //
    // FLAG -- set up to iterate through multiple viewports if there are. 
    //
    clearTimeout(resizeTimer);
    
    console.log('Running puiResize for ' + rp);
    resizeTimer = setTimeout(function () {
        
        gdlAjax(null, 'args=' + encode64('(:|iid| '+ doublequote + gdliid + doublequote + ' :|bashee| (:%rp% ' + rp + ') :|function| :set-slot! :|arguments| (:viewport-dimensions (:width ' + (document.getElementById('viewport').getBoundingClientRect().width) +  ' :length ' + (document.getElementById('viewport').getBoundingClientRect().height) + ')))'), true );}, 250);
}





//
// debouncing technique from https://css-tricks.com/snippets/jquery/done-resizing-event/.
//
// More general debouncing function here: https://davidwalsh.name/javascript-debounce-function
//
var puiresizeTimer;

function gdlpuiResize()
{

    console.log('Running gdlpuiResize');
    // if  (document.getElementById('x3dom-1'))
    // {}
    //else
    //{
	      
    clearTimeout(puiresizeTimer);
    puiresizeTimer = setTimeout(function () {
    
	gdlAjax(null, 'args=' + encode64('(:|iid| '+ doublequote + gdliid + doublequote + ' :|bashee| (:%rp% nil) :|function| :set-slot! :|arguments| (:viewport-dimensions (:width ' + (document.getElementById('viewport').getBoundingClientRect().width) +  ' :length ' + (document.getElementById('viewport').getBoundingClientRect().height) + ')))'), true );}, 250);

// }

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


<!DOCTYPE html>
<html class="no-touch" lang="en"><head>
<meta http-equiv="content-type" content="text/html; charset=UTF-8">
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Homework Checker</title>
  <style>
    header {
      padding: 10px;
      display: flex;
      align-items: center;
      justify-content: center;
      font-size: 20px;
      border-bottom: 1px solid #888;
    }
    .download-button {
      margin: 20px;
      color: white;
      background: #e3301b;
      padding: 10px;
      text-decoration: none;
      font-family: sans-serif;
      user-select: none;
    }
    .download-button:hover {
      text-decoration: underline;
    }
    h1 {
      font-size: 20px;
      font-variant: small-caps;
    }
    h2 {
      font-size: 16px;
    }
    main {
      padding: 50px;
      max-width: 600px;
    }
    pre {
      margin-left: 1em;
      padding-left: 1em;
      border-left: 1px solid #888;
    }
  </style>
  </head>
  <body>
    <main><h1 id="homework-0">Homework #0</h1>
<h2 id="task-1">Task 1</h2>
<ol type="1">
<li><p>Create a module named <code>HW0.T1</code> and define the following type in it:</p>
<pre><code>data a &lt;-&gt; b = Iso (a -&gt; b) (b -&gt; a)

flipIso :: (a &lt;-&gt; b) -&gt; (b &lt;-&gt; a)
flipIso (Iso f g) = Iso g f

runIso :: (a &lt;-&gt; b) -&gt; (a -&gt; b)
runIso (Iso f _) = f</code></pre></li>
<li><p>Implement the following functions and isomorphisms:</p>
<pre><code>distrib :: Either a (b, c) -&gt; (Either a b, Either a c)
assocPair :: (a, (b, c)) &lt;-&gt; ((a, b), c)
assocEither :: Either a (Either b c) &lt;-&gt; Either (Either a b) c</code></pre></li>
</ol>
<h2 id="task-2">Task 2</h2>
<ol type="1">
<li><p>Create a module named <code>HW0.T2</code> and define the following type in it:</p>
<pre><code>type Not a = a -&gt; Void</code></pre></li>
<li><p>Implement the following functions and isomorphisms:</p>
<pre><code>doubleNeg :: a -&gt; Not (Not a)
reduceTripleNeg :: Not (Not (Not a)) -&gt; Not a</code></pre></li>
</ol>
<h2 id="task-3">Task 3</h2>
<ol type="1">
<li><p>Create a module named <code>HW0.T3</code> and define the following combinators in it:</p>
<pre><code>s :: (a -&gt; b -&gt; c) -&gt; (a -&gt; b) -&gt; (a -&gt; c)
s f g x = f x (g x)

k :: a -&gt; b -&gt; a
k x y = x</code></pre></li>
<li><p>Using <em>only those combinators</em> and function application (i.e.&nbsp;no lambdas, pattern matching, and so on) define the following additional combinators:</p>
<pre><code>i :: a -&gt; a
compose :: (b -&gt; c) -&gt; (a -&gt; b) -&gt; (a -&gt; c)
contract :: (a -&gt; a -&gt; b) -&gt; (a -&gt; b)
permute :: (a -&gt; b -&gt; c) -&gt; (b -&gt; a -&gt; c)</code></pre>
<p>For example:</p>
<pre><code>i x = x         -- No (parameters on the LHS disallowed)
i = \x -&gt; x     -- No (lambdas disallowed)
i = Prelude.id  -- No (only use s and k)
i = s k k       -- OK
i = (s k) k     -- OK (parentheses for grouping allowed)</code></pre></li>
</ol>
<h2 id="task-4">Task 4</h2>
<ol type="1">
<li><p>Create a module named <code>HW0.T4</code>.</p></li>
<li><p>Using the <code>fix</code> combinator from the <code>Data.Function</code> module define the following functions:</p>
<pre><code>repeat' :: a -&gt; [a]             -- behaves like Data.List.repeat
map' :: (a -&gt; b) -&gt; [a] -&gt; [b]  -- behaves like Data.List.map
fib :: Natural -&gt; Natural       -- computes the n-th Fibonacci number
fac :: Natural -&gt; Natural       -- computes the factorial</code></pre>
<p>Do not use explicit recursion. For example:</p>
<pre><code>repeat' = Data.List.repeat     -- No (obviously)
repeat' x = x : repeat' x      -- No (explicit recursion disallowed)
repeat' x = fix (x:)           -- OK</code></pre></li>
</ol>
<h2 id="task-5">Task 5</h2>
<ol type="1">
<li><p>Create a module named <code>HW0.T5</code> and define the following type in it:</p>
<pre><code>type Nat a = (a -&gt; a) -&gt; a -&gt; a</code></pre></li>
<li><p>Implement the following functions:</p>
<pre><code>nz :: Nat a
ns :: Nat a -&gt; Nat a

nplus, nmult :: Nat a -&gt; Nat a -&gt; Nat a

nFromNatural :: Natural -&gt; Nat a
nToNum :: Num a =&gt; Nat a -&gt; a</code></pre></li>
<li><p>The following equations must hold:</p>
<pre><code>nToNum nz       ==  0
nToNum (ns x)   ==  1 + nToNum x

nToNum (nplus a b)   ==   nToNum a + nToNum b
nToNum (nmult a b)   ==   nToNum a * nToNum b</code></pre></li>
</ol>
<h2 id="task-6">Task 6</h2>
<ol type="1">
<li><p>Create a module named <code>HW0.T6</code> and define the following values in it:</p>
<pre><code>a = distrib (Left ("AB" ++ "CD" ++ "EF"))     -- distrib from HW0.T1
b = map isSpace "Hello, World"
c = if 1 &gt; 0 || error "X" then "Y" else "Z"</code></pre></li>
<li><p>Determine the WHNF (weak head normal form) of these values:</p>
<pre><code>a_whnf = ...
b_whnf = ...
c_whnf = ...</code></pre></li>
</ol>

<textarea id="BFI_DATA" style="width: 1px; height: 1px; display: none;"></textarea><title> </title><div id="WidgetFloaterPanels" translate="no" style="display: none; text-align: left; direction: ltr; visibility: hidden;" class="LTRStyle"> <div id="WidgetFloater" style="display: none" onmouseover="Microsoft.Translator.OnMouseOverFloater()" onmouseout="Microsoft.Translator.OnMouseOutFloater()"> <div id="WidgetLogoPanel"> <span id="WidgetTranslateWithSpan"><span>TRANSLATE with </span><img id="FloaterLogo"></span> <span id="WidgetCloseButton" title="Exit Translation" onclick="Microsoft.Translator.FloaterOnClose()">x</span></div> <div id="LanguageMenuPanel"> <div class="DDStyle_outer"><input name="LanguageMenu_svid" type="text" id="LanguageMenu_svid" style="display:none;" autocomplete="on" value="en" onclick="this.select()"> <input name="LanguageMenu_textid" type="text" id="LanguageMenu_textid" style="display:none;" autocomplete="on" onclick="this.select()"> <span onselectstart="return false" tabindex="0" class="DDStyle" id="__LanguageMenu_header" onclick="return LanguageMenu &amp;&amp; !LanguageMenu.Show('__LanguageMenu_popup', event);" onkeydown="return LanguageMenu &amp;&amp; !LanguageMenu.Show('__LanguageMenu_popup', event);">English</span> <div style="position:relative;text-align:left;left:0;"><div style="position:absolute;width:;left:0px;"><div class="DDStyle" style="display:none;" id="__LanguageMenu_popup"> <table id="LanguageMenu" border="0"> <tbody><tr> <td><a tabindex="-1" onclick="return LanguageMenu.onclick('ar');" ondragstart="LanguageMenu.ondragstart(event);" href="#ar">Arabic</a></td><td><a tabindex="-1" onclick="return LanguageMenu.onclick('he');" ondragstart="LanguageMenu.ondragstart(event);" href="#he">Hebrew</a></td><td><a tabindex="-1" onclick="return LanguageMenu.onclick('pl');" ondragstart="LanguageMenu.ondragstart(event);" href="#pl">Polish</a></td> </tr><tr> <td><a tabindex="-1" onclick="return LanguageMenu.onclick('bg');" ondragstart="LanguageMenu.ondragstart(event);" href="#bg">Bulgarian</a></td><td><a tabindex="-1" onclick="return LanguageMenu.onclick('hi');" ondragstart="LanguageMenu.ondragstart(event);" href="#hi">Hindi</a></td><td><a tabindex="-1" onclick="return LanguageMenu.onclick('pt');" ondragstart="LanguageMenu.ondragstart(event);" href="#pt">Portuguese</a></td> </tr><tr> <td><a tabindex="-1" onclick="return LanguageMenu.onclick('ca');" ondragstart="LanguageMenu.ondragstart(event);" href="#ca">Catalan</a></td><td><a tabindex="-1" onclick="return LanguageMenu.onclick('mww');" ondragstart="LanguageMenu.ondragstart(event);" href="#mww">Hmong Daw</a></td><td><a tabindex="-1" onclick="return LanguageMenu.onclick('ro');" ondragstart="LanguageMenu.ondragstart(event);" href="#ro">Romanian</a></td> </tr><tr> <td><a tabindex="-1" onclick="return LanguageMenu.onclick('zh-CHS');" ondragstart="LanguageMenu.ondragstart(event);" href="#zh-CHS">Chinese Simplified</a></td><td><a tabindex="-1" onclick="return LanguageMenu.onclick('hu');" ondragstart="LanguageMenu.ondragstart(event);" href="#hu">Hungarian</a></td><td><a tabindex="-1" onclick="return LanguageMenu.onclick('ru');" ondragstart="LanguageMenu.ondragstart(event);" href="#ru">Russian</a></td> </tr><tr> <td><a tabindex="-1" onclick="return LanguageMenu.onclick('zh-CHT');" ondragstart="LanguageMenu.ondragstart(event);" href="#zh-CHT">Chinese Traditional</a></td><td><a tabindex="-1" onclick="return LanguageMenu.onclick('id');" ondragstart="LanguageMenu.ondragstart(event);" href="#id">Indonesian</a></td><td><a tabindex="-1" onclick="return LanguageMenu.onclick('sk');" ondragstart="LanguageMenu.ondragstart(event);" href="#sk">Slovak</a></td> </tr><tr> <td><a tabindex="-1" onclick="return LanguageMenu.onclick('cs');" ondragstart="LanguageMenu.ondragstart(event);" href="#cs">Czech</a></td><td><a tabindex="-1" onclick="return LanguageMenu.onclick('it');" ondragstart="LanguageMenu.ondragstart(event);" href="#it">Italian</a></td><td><a tabindex="-1" onclick="return LanguageMenu.onclick('sl');" ondragstart="LanguageMenu.ondragstart(event);" href="#sl">Slovenian</a></td> </tr><tr> <td><a tabindex="-1" onclick="return LanguageMenu.onclick('da');" ondragstart="LanguageMenu.ondragstart(event);" href="#da">Danish</a></td><td><a tabindex="-1" onclick="return LanguageMenu.onclick('ja');" ondragstart="LanguageMenu.ondragstart(event);" href="#ja">Japanese</a></td><td><a tabindex="-1" onclick="return LanguageMenu.onclick('es');" ondragstart="LanguageMenu.ondragstart(event);" href="#es">Spanish</a></td> </tr><tr> <td><a tabindex="-1" onclick="return LanguageMenu.onclick('nl');" ondragstart="LanguageMenu.ondragstart(event);" href="#nl">Dutch</a></td><td><a tabindex="-1" onclick="return LanguageMenu.onclick('tlh');" ondragstart="LanguageMenu.ondragstart(event);" href="#tlh">Klingon</a></td><td><a tabindex="-1" onclick="return LanguageMenu.onclick('sv');" ondragstart="LanguageMenu.ondragstart(event);" href="#sv">Swedish</a></td> </tr><tr> <td><a tabindex="-1" onclick="return LanguageMenu.onclick('en');" ondragstart="LanguageMenu.ondragstart(event);" href="#en">English</a></td><td><a tabindex="-1" onclick="return LanguageMenu.onclick('ko');" ondragstart="LanguageMenu.ondragstart(event);" href="#ko">Korean</a></td><td><a tabindex="-1" onclick="return LanguageMenu.onclick('th');" ondragstart="LanguageMenu.ondragstart(event);" href="#th">Thai</a></td> </tr><tr> <td><a tabindex="-1" onclick="return LanguageMenu.onclick('et');" ondragstart="LanguageMenu.ondragstart(event);" href="#et">Estonian</a></td><td><a tabindex="-1" onclick="return LanguageMenu.onclick('lv');" ondragstart="LanguageMenu.ondragstart(event);" href="#lv">Latvian</a></td><td><a tabindex="-1" onclick="return LanguageMenu.onclick('tr');" ondragstart="LanguageMenu.ondragstart(event);" href="#tr">Turkish</a></td> </tr><tr> <td><a tabindex="-1" onclick="return LanguageMenu.onclick('fi');" ondragstart="LanguageMenu.ondragstart(event);" href="#fi">Finnish</a></td><td><a tabindex="-1" onclick="return LanguageMenu.onclick('lt');" ondragstart="LanguageMenu.ondragstart(event);" href="#lt">Lithuanian</a></td><td><a tabindex="-1" onclick="return LanguageMenu.onclick('uk');" ondragstart="LanguageMenu.ondragstart(event);" href="#uk">Ukrainian</a></td> </tr><tr> <td><a tabindex="-1" onclick="return LanguageMenu.onclick('fr');" ondragstart="LanguageMenu.ondragstart(event);" href="#fr">French</a></td><td><a tabindex="-1" onclick="return LanguageMenu.onclick('ms');" ondragstart="LanguageMenu.ondragstart(event);" href="#ms">Malay</a></td><td><a tabindex="-1" onclick="return LanguageMenu.onclick('ur');" ondragstart="LanguageMenu.ondragstart(event);" href="#ur">Urdu</a></td> </tr><tr> <td><a tabindex="-1" onclick="return LanguageMenu.onclick('de');" ondragstart="LanguageMenu.ondragstart(event);" href="#de">German</a></td><td><a tabindex="-1" onclick="return LanguageMenu.onclick('mt');" ondragstart="LanguageMenu.ondragstart(event);" href="#mt">Maltese</a></td><td><a tabindex="-1" onclick="return LanguageMenu.onclick('vi');" ondragstart="LanguageMenu.ondragstart(event);" href="#vi">Vietnamese</a></td> </tr><tr> <td><a tabindex="-1" onclick="return LanguageMenu.onclick('el');" ondragstart="LanguageMenu.ondragstart(event);" href="#el">Greek</a></td><td><a tabindex="-1" onclick="return LanguageMenu.onclick('no');" ondragstart="LanguageMenu.ondragstart(event);" href="#no">Norwegian</a></td><td><a tabindex="-1" onclick="return LanguageMenu.onclick('cy');" ondragstart="LanguageMenu.ondragstart(event);" href="#cy">Welsh</a></td> </tr><tr> <td><a tabindex="-1" onclick="return LanguageMenu.onclick('ht');" ondragstart="LanguageMenu.ondragstart(event);" href="#ht">Haitian Creole</a></td><td><a tabindex="-1" onclick="return LanguageMenu.onclick('fa');" ondragstart="LanguageMenu.ondragstart(event);" href="#fa">Persian</a></td><td></td> </tr> </tbody></table> <img alt="" style="height:7px;width:17px;border-width:0px;left:20px;"> </div></div></div></div> <script type="text/javascript"> var LanguageMenu; var LanguageMenu_keys=["ar","bg","ca","zh-CHS","zh-CHT","cs","da","nl","en","et","fi","fr","de","el","ht","he","hi","mww","hu","id","it","ja","tlh","ko","lv","lt","ms","mt","no","fa","pl","pt","ro","ru","sk","sl","es","sv","th","tr","uk","ur","vi","cy"]; var LanguageMenu_values=["Arabic","Bulgarian","Catalan","Chinese Simplified","Chinese Traditional","Czech","Danish","Dutch","English","Estonian","Finnish","French","German","Greek","Haitian Creole","Hebrew","Hindi","Hmong Daw","Hungarian","Indonesian","Italian","Japanese","Klingon","Korean","Latvian","Lithuanian","Malay","Maltese","Norwegian","Persian","Polish","Portuguese","Romanian","Russian","Slovak","Slovenian","Spanish","Swedish","Thai","Turkish","Ukrainian","Urdu","Vietnamese","Welsh"]; var LanguageMenu_callback=function(){ }; var LanguageMenu_popupid='__LanguageMenu_popup'; </script> </div> <div id="CTFLinksPanel"> <span id="ExternalLinksPanel"><a id="HelpLink" title="Help" target="_blank" href="https://go.microsoft.com/?linkid=9722454"> <img id="HelpImg"></a> <a id="EmbedLink" href="javascript:Microsoft.Translator.FloaterShowEmbed()" title="Get this widget for your own site"> <img id="EmbedImg"></a> <a id="ShareLink" title="Share translated page with friends" href="javascript:Microsoft.Translator.FloaterShowSharePanel()"> <img id="ShareImg"></a> </span> </div> <div id="FloaterProgressBar"> <span id="ProgressFill"></span> </div> </div> <div id="WidgetFloaterCollapsed" style="display: none" onmouseover="Microsoft.Translator.OnMouseOverFloater()"> <span>TRANSLATE with </span><img id="CollapsedLogoImg"></div> <div id="FloaterSharePanel" style="display: none"> <div id="ShareTextDiv"> <span id="ShareTextSpan"> COPY THE URL BELOW </span> </div> <div id="ShareTextboxDiv"> <input name="ShareTextbox" type="text" id="ShareTextbox" readonly="readonly" onclick="this.select()"> <!--a id="TwitterLink" title="Share on Twitter"> <img id="TwitterImg" /></a> <a-- id="FacebookLink" title="Share on Facebook"> <img id="FacebookImg" /></a--> <a id="EmailLink" title="Email this translation"> <img id="EmailImg"></a> </div> <div id="ShareFooter"> <span id="ShareHelpSpan"><a id="ShareHelpLink"> <img id="ShareHelpImg"></a></span> <span id="ShareBackSpan"><a id="ShareBack" href="javascript:Microsoft.Translator.FloaterOnShareBackClick()" title="Back To Translation"> Back</a></span> </div> <input name="EmailSubject" type="hidden" id="EmailSubject" value="Check out this page in {0} translated from {1}"> <input name="EmailBody" type="hidden" id="EmailBody" value="Translated: {0}%0d%0aOriginal: {1}%0d%0a%0d%0aAutomatic translation powered by Microsoft® Translator%0d%0ahttp://www.bing.com/translator?ref=MSTWidget"> <input type="hidden" id="ShareHelpText" value="This link allows visitors to launch this page and automatically translate it to {0}."> </div> <div id="FloaterEmbed" style="display: none"> <div id="EmbedTextDiv"> <span id="EmbedTextSpan">EMBED THE SNIPPET BELOW IN YOUR SITE</span> <a id="EmbedHelpLink" title="Copy this code and place it into your HTML."> <img id="EmbedHelpImg"></a> </div> <div id="EmbedTextboxDiv"> <input name="EmbedSnippetTextBox" type="text" id="EmbedSnippetTextBox" readonly="readonly" value="&lt;div id='MicrosoftTranslatorWidget' class='Dark' style='color:white;background-color:#555555'&gt;&lt;/div&gt;&lt;script type='text/javascript'&gt;setTimeout(function(){var s=document.createElement('script');s.type='text/javascript';s.charset='UTF-8';s.src=((location &amp;&amp; location.href &amp;&amp; location.href.indexOf('https') == 0)?'https://ssl.microsofttranslator.com':'http://www.microsofttranslator.com')+'/ajax/v3/WidgetV3.ashx?siteData=ueOIGRSKkd965FeEGM5JtQ**&amp;ctf=true&amp;ui=true&amp;settings=manual&amp;from=en';var p=document.getElementsByTagName('head')[0]||document.documentElement;p.insertBefore(s,p.firstChild); },0);&lt;/script&gt;" onclick="this.select()"> </div> <div id="EmbedNoticeDiv"><span id="EmbedNoticeSpan">Enable collaborative features and customize widget: <a href="http://www.bing.com/widget/translator" target="_blank">Bing Webmaster Portal</a></span></div> <div id="EmbedFooterDiv"><span id="EmbedBackSpan"><a href="javascript:Microsoft.Translator.FloaterOnEmbedBackClick()" title="Back To Translation">Back</a></span></div> </div> <script type="text/javascript"> var intervalId = setInterval(function () { if (MtPopUpList) { LanguageMenu = new MtPopUpList(); var langMenu = document.getElementById(LanguageMenu_popupid); var origLangDiv = document.createElement("div"); origLangDiv.id = "OriginalLanguageDiv"; origLangDiv.innerHTML = "<span id='OriginalTextSpan'>ORIGINAL: </span><span id='OriginalLanguageSpan'></span>"; langMenu.appendChild(origLangDiv); LanguageMenu.Init('LanguageMenu', LanguageMenu_keys, LanguageMenu_values, LanguageMenu_callback, LanguageMenu_popupid); window["LanguageMenu"] = LanguageMenu; clearInterval(intervalId); } }, 1); </script> </div></body></html>
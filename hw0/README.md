# Homework-0
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com//fp-homework/blob/master/hw1/LICENSE)

<!DOCTYPE html>
<html class="no-touch" lang="en">
  <head>
    <meta http-equiv="content-type" content="text/html; charset=UTF-8">
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
  </head>
  <body>
    <main>
    <h2 id="task-1">Task 1</h2>
    <ol type="1">
      <li>
        <p>Create a module named <code>HW0.T1</code> and define the following type in it:</p>
        <pre><code>data a &lt;-&gt; b = Iso (a -&gt; b) (b -&gt; a)

flipIso :: (a &lt;-&gt; b) -&gt; (b &lt;-&gt; a)
flipIso (Iso f g) = Iso g f

runIso :: (a &lt;-&gt; b) -&gt; (a -&gt; b)
runIso (Iso f _) = f</code></pre>
      </li>
      <li>
        <p>Implement the following functions and isomorphisms:</p>
        <pre><code>distrib :: Either a (b, c) -&gt; (Either a b, Either a c)
assocPair :: (a, (b, c)) &lt;-&gt; ((a, b), c)
assocEither :: Either a (Either b c) &lt;-&gt; Either (Either a b) c</code></pre>
      </li>
    </ol>
    <h2 id="task-2">Task 2</h2>
    <ol type="1">
      <li>
        <p>Create a module named <code>HW0.T2</code> and define the following type in it:</p>
        <pre><code>type Not a = a -&gt; Void</code></pre>
      </li>
      <li>
        <p>Implement the following functions and isomorphisms:</p>
        <pre><code>doubleNeg :: a -&gt; Not (Not a)
reduceTripleNeg :: Not (Not (Not a)) -&gt; Not a</code></pre>
      </li>
    </ol>
    <h2 id="task-3">Task 3</h2>
    <ol type="1">
      <li>
        <p>Create a module named <code>HW0.T3</code> and define the following combinators in it:</p>
        <pre><code>s :: (a -&gt; b -&gt; c) -&gt; (a -&gt; b) -&gt; (a -&gt; c)
s f g x = f x (g x)

k :: a -&gt; b -&gt; a
k x y = x</code></pre>
      </li>
      <li>
        <p>Using <em>only those combinators</em> and function application (i.e.&nbsp;no lambdas, pattern matching, and so on) define the following additional combinators:</p>
        <pre><code>i :: a -&gt; a
compose :: (b -&gt; c) -&gt; (a -&gt; b) -&gt; (a -&gt; c)
contract :: (a -&gt; a -&gt; b) -&gt; (a -&gt; b)
permute :: (a -&gt; b -&gt; c) -&gt; (b -&gt; a -&gt; c)</code></pre>
        <p>For example:</p>
        <pre><code>i x = x         -- No (parameters on the LHS disallowed)
i = \x -&gt; x     -- No (lambdas disallowed)
i = Prelude.id  -- No (only use s and k)
i = s k k       -- OK
i = (s k) k     -- OK (parentheses for grouping allowed)</code></pre>
      </li>
    </ol>
    <h2 id="task-4">Task 4</h2>
    <ol type="1">
      <li>
        <p>Create a module named <code>HW0.T4</code>.</p>
      </li>
      <li>
        <p>Using the <code>fix</code> combinator from the <code>Data.Function</code> module define the following functions:</p>
        <pre><code>repeat' :: a -&gt; [a]             -- behaves like Data.List.repeat
map' :: (a -&gt; b) -&gt; [a] -&gt; [b]  -- behaves like Data.List.map
fib :: Natural -&gt; Natural       -- computes the n-th Fibonacci number
fac :: Natural -&gt; Natural       -- computes the factorial</code></pre>
        <p>Do not use explicit recursion. For example:</p>
        <pre><code>repeat' = Data.List.repeat     -- No (obviously)
repeat' x = x : repeat' x      -- No (explicit recursion disallowed)
repeat' x = fix (x:)           -- OK</code></pre>
      </li>
    </ol>
    <h2 id="task-5">Task 5</h2>
    <ol type="1">
      <li>
        <p>Create a module named <code>HW0.T5</code> and define the following type in it:</p>
        <pre><code>type Nat a = (a -&gt; a) -&gt; a -&gt; a</code></pre>
      </li>
      <li>
        <p>Implement the following functions:</p>
        <pre><code>nz :: Nat a
ns :: Nat a -&gt; Nat a

nplus, nmult :: Nat a -&gt; Nat a -&gt; Nat a

nFromNatural :: Natural -&gt; Nat a
nToNum :: Num a =&gt; Nat a -&gt; a</code></pre>
      </li>
      <li>
        <p>The following equations must hold:</p>
        <pre><code>nToNum nz       ==  0
nToNum (ns x)   ==  1 + nToNum x

nToNum (nplus a b)   ==   nToNum a + nToNum b
nToNum (nmult a b)   ==   nToNum a * nToNum b</code></pre>
      </li>
    </ol>
    <h2 id="task-6">Task 6</h2>
    <ol type="1">
      <li>
        <p>Create a module named <code>HW0.T6</code> and define the following values in it:</p>
        <pre><code>a = distrib (Left ("AB" ++ "CD" ++ "EF"))     -- distrib from HW0.T1
b = map isSpace "Hello, World"
c = if 1 &gt; 0 || error "X" then "Y" else "Z"</code></pre>
      </li>
      <li>
        <p>Determine the WHNF (weak head normal form) of these values:</p>
        <pre><code>a_whnf = ...
b_whnf = ...
c_whnf = ...</code></pre>
      </li>
    </ol>
    </div>
  </body>
</html>

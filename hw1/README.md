# Homework #1

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
      <h2 id="task-1-1">Task 1</h2>
      <ol type="1">
        <li>
          <p>Create a module named <code>HW1.T1</code> and define the following data type in it:</p>
          <pre><code>data Day = Monday | Tuesday | ... | Sunday</code></pre>
          <p>(Obviously, fill in the <code>...</code> with the rest of the week days).</p>
          <p>Do not derive <code>Enum</code> for <code>Day</code>, as the derived <code>toEnum</code> is partial:</p>
          <pre><code>ghci&gt; toEnum 42 :: Day
*** Exception: toEnum{Day}: tag (42) is outside of enumeration's range (0,6)</code></pre>
        </li>
        <li>
          <p>Implement the following functions:</p>
          <pre><code>-- | Returns the day that follows the day of the week given as input.
nextDay :: Day -&gt; Day

-- | Returns the day of the week after a given number of days has passed.
afterDays :: Natural -&gt; Day -&gt; Day

-- | Checks if the day is on the weekend.
isWeekend :: Day -&gt; Bool

-- | Computes the number of days until Friday.
daysToParty :: Day -&gt; Natural</code></pre>
          <p>In <code>daysToParty</code>, if it is already <code>Friday</code>, the party can start immediately, we don’t have to wait for the next week (i.e.&nbsp;return <code>0</code> rather than <code>7</code>).</p>
          <p><small>Good job if you spotted that this task is a perfect fit for 
            modular arithmetic, but that is not the point of the exercise. The 
            functions must be implemented by operating on <code>Day</code> values directly, without conversion to a numeric representation. </small>
          </p>
        </li>
      </ol>
      <h2 id="task-2-1">Task 2</h2>
      <ol type="1">
        <li>
          <p>Create a module named <code>HW1.T2</code> and define the following data type in it:</p>
          <pre><code>data N = Z | S N</code></pre>
        </li>
        <li>
          <p>Implement the following functions:</p>
          <pre><code>nplus :: N -&gt; N -&gt; N        -- addition
nmult :: N -&gt; N -&gt; N        -- multiplication
nsub :: N -&gt; N -&gt; Maybe N   -- subtraction     (Nothing if result is negative)
ncmp :: N -&gt; N -&gt; Ordering  -- comparison      (Do not derive Ord)</code></pre>
          <p>The operations must be implemented without using built-in numbers (<code>Int</code>, <code>Integer</code>, <code>Natural</code>, and such).</p>
        </li>
        <li>
          <p>Implement the following functions:</p>
          <pre><code>nFromNatural :: Natural -&gt; N
nToNum :: Num a =&gt; N -&gt; a</code></pre>
        </li>
        <li>
          <p>(Advanced) Implement the following functions:</p>
          <pre><code>nEven, nOdd :: N -&gt; Bool    -- parity checking
ndiv :: N -&gt; N -&gt; N         -- integer division
nmod :: N -&gt; N -&gt; N         -- modulo operation</code></pre>
          <p>The operations must be implemented without using built-in numbers.</p>
          <p><small>In <code>ndiv</code> and <code>nmod</code>, the behavior in 
            case of division by zero is not specified (you can throw an exception, 
            go into an inifnite loop, or delete all files on the computer).</small>
          </p>
        </li>
      </ol>
      <h2 id="task-3-1">Task 3</h2>
      <ol type="1">
        <li>
          <p>Create a module named <code>HW1.T3</code> and define the following data type in it:</p>
          <pre><code>data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)</code></pre>
          <p>The <code>Meta</code> field must store additional information about 
            the subtree that can be accessed in constant time, for example its size.
            You can use <code>Int</code>, <code>(Int, Int)</code>, or a custom data structure:
          </p>
          <pre><code>type Meta = Int         -- OK
data Meta = M Int Int   -- OK</code></pre>
          <p>Functions operating on this tree must maintain the following invariants:</p>
          <ol type="1">
            <li><strong>Sorted</strong>: The elements in the left subtree are less 
              than the head element of a branch, and the elements in the right subtree
              are greater.
            </li>
            <li><strong>Unique</strong>: There are no duplicate elements in the tree (follows from <strong>Sorted</strong>).</li>
            <li><strong>CachedSize</strong>: The size of the tree is cached in the <code>Meta</code> field for constant-time access.</li>
            <li>
              (Advanced) <strong>Balanced</strong>: The tree is balanced according to one of the following strategies:
              <ul>
                <li><strong>SizeBalanced</strong>: For any given <code>Branch _ l _ r</code>, the ratio between the size of <code>l</code> and the size of <code>r</code> never exceeds <code>3</code>.</li>
                <li><strong>HeightBalanced</strong>: For any given <code>Branch _ l _ r</code>, the difference between the height of <code>l</code> and the height of <code>r</code> never exceeds <code>1</code>.</li>
              </ul>
            </li>
          </ol>
          <p>These invariants enable efficient processing of the tree.</p>
        </li>
        <li>
          <p>Implement the following functions:</p>
          <pre><code>-- | Size of the tree, O(1).
tsize :: Tree a -&gt; Int

-- | Depth of the tree.
tdepth :: Tree a -&gt; Int

-- | Check if the element is in the tree, O(log n)
tmember :: Ord a =&gt; a -&gt; Tree a -&gt; Bool

-- | Insert an element into the tree, O(log n)
tinsert :: Ord a =&gt; a -&gt; Tree a -&gt; Tree a

-- | Build a tree from a list, O(n log n)
tFromList :: Ord a =&gt; [a] -&gt; Tree a</code></pre>
          <p>Tip 1: in order to maintain the <strong>CachedSize</strong> invariant, define a helper function:</p>
          <pre><code>mkBranch :: Tree a -&gt; a -&gt; Tree a -&gt; Tree a</code></pre>
          <p>Tip 2: the <strong>Balanced</strong> invariant is the hardest to maintain, so implement it last. Search for “tree rotation”.</p>
        </li>
      </ol>
      <h2 id="task-4-1">Task 4</h2>
      <ol type="1">
        <li>
          <p>Create a module named <code>HW1.T4</code>.</p>
        </li>
        <li>
          <p>Using the <code>Tree</code> data type from <code>HW1.T3</code>, define the following function:</p>
          <pre><code>tfoldr :: (a -&gt; b -&gt; b) -&gt; b -&gt; Tree a -&gt; b</code></pre>
          <p>It must collect the elements in order:</p>
          <pre><code>treeToList :: Tree a -&gt; [a]    -- output list is sorted
treeToList = tfoldr (:) []</code></pre>
          <p>This follows from the <strong>Sorted</strong> invariant.</p>
          <p>You are encouraged to define <code>tfoldr</code> in an efficient manner, doing only a single pass over the tree and without constructing intermediate lists.</p>
        </li>
      </ol>
      <h2 id="task-5-1">Task 5</h2>
      <ol type="1">
        <li>
          <p>Create a module named <code>HW1.T5</code>.</p>
        </li>
        <li>
          <p>Implement the following function:</p>
          <pre><code>splitOn :: Eq a =&gt; a -&gt; [a] -&gt; NonEmpty [a]</code></pre>
          <p>Conceptually, it splits a list into sublists by a separator:</p>
          <pre><code>ghci&gt; splitOn '/' "path/to/file"
["path", "to", "file"]

ghci&gt; splitOn '/' "path/with/trailing/slash/"
["path", "with", "trailing", "slash", ""]</code></pre>
          <p>Due to the use of <code>NonEmpty</code> to enforce that there is at least one sublist in the output, the actual GHCi result will look slightly differently:</p>
          <pre><code>ghci&gt; splitOn '/' "path/to/file"
"path" :| ["to","file"]</code></pre>
          <p>Do not let that confuse you. The first element is not in any way special.</p>
        </li>
        <li>
          <p>Implement the following function:</p>
          <pre><code>joinWith :: a -&gt; NonEmpty [a] -&gt; [a]</code></pre>
          <p>It must be the inverse of <code>splitOn</code>, so that:</p>
          <pre><code>(joinWith sep . splitOn sep)  ≡  id</code></pre>
          <p>Example usage:</p>
          <pre><code>ghci&gt; "import " ++ joinWith '.' ("Data" :| "List" : "NonEmpty" : [])
"import Data.List.NonEmpty"</code></pre>
        </li>
      </ol>
      <h2 id="task-6-1">Task 6</h2>
      <ol type="1">
        <li>
          <p>Create a module named <code>HW1.T6</code>.</p>
        </li>
        <li>
          <p>Using <code>Foldable</code> methods <em>only</em>, implement the following function:</p>
          <pre><code>mcat :: Monoid a =&gt; [Maybe a] -&gt; a</code></pre>
          <p>Example usage:</p>
          <pre><code>ghci&gt; mcat [Just "mo", Nothing, Nothing, Just "no", Just "id"]
"monoid"

ghci&gt; Data.Monoid.getSum $ mcat [Nothing, Just 2, Nothing, Just 40]
42</code></pre>
        </li>
        <li>
          <p>Using <code>foldMap</code> to consume the list, implement the following function:</p>
          <pre><code>epart :: (Monoid a, Monoid b) =&gt; [Either a b] -&gt; (a, b)</code></pre>
          <p>Example usage:</p>
          <pre><code>ghci&gt; epart [Left (Sum 3), Right [1,2,3], Left (Sum 5), Right [4,5]]
(Sum {getSum = 8},[1,2,3,4,5])</code></pre>
        </li>
      </ol>
      <h2 id="task-7">Task 7</h2>
      <ol type="1">
        <li>
          <p>Create a module named <code>HW1.T7</code>.</p>
        </li>
        <li>
          <p>Define the following data type and a lawful <code>Semigroup</code> instance for it:</p>
          <pre><code>data ListPlus a = a :+ ListPlus a | Last a
infixr 5 :+</code></pre>
        </li>
        <li>
          <p>Define the following data type and a lawful <code>Semigroup</code> instance for it:</p>
          <pre><code>data Inclusive a b = This a | That b | Both a b</code></pre>
          <p>The instance must not discard any values:</p>
          <pre><code>This i  &lt;&gt;  This j  =  This (i &lt;&gt; j)   -- OK
This i  &lt;&gt;  This _  =  This i          -- This is not the Semigroup you're looking for.</code></pre>
        </li>
        <li>
          <p>Define the following data type:</p>
          <pre><code>newtype DotString = DS String</code></pre>
          <p>Implement a <code>Semigroup</code> instance for it, such that the strings are concatenated with a dot:</p>
          <pre><code>ghci&gt; DS "person" &lt;&gt; DS "address" &lt;&gt; DS "city"
DS "person.address.city"</code></pre>
          <p>Implement a <code>Monoid</code> instance for it using <code>DS ""</code> as the identity element. Make sure that the laws hold:</p>
          <pre><code>mempty &lt;&gt; a  ≡  a
a &lt;&gt; mempty  ≡  a</code></pre>
        </li>
        <li>
          <p>Define the following data type:</p>
          <pre><code>newtype Fun a = F (a -&gt; a)</code></pre>
          <p>Implement lawful <code>Semigroup</code> and <code>Monoid</code> instances for it.</p>
        </li>
      </ol>
    </main>
  </body>
</html>


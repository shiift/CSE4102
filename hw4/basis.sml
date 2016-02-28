(*********************************************************************
 * A stream is either empty, or it contains one element
 * and a thunk that evaluates to another stream.
 ********************************************************************)
datatype 'a Stream
  = Empty
  | Stream of 'a * (unit -> 'a Stream)
  ;
(*********************************************************************
 * Simple predicate function to determine if a stream is empty.
 ********************************************************************)
fun isEmpty Empty = true
  | isEmpty _ = false
  ;
exception StreamEmpty;
(*********************************************************************
 * Returns the head of the stream, but only if the stream
 * is not empty.
 ********************************************************************)
fun head (Stream (x, _)) = x
  | head _ = raise StreamEmpty
  ;
(*********************************************************************
 * Forces and returns the tail of the stream (meaning, it returns
 * the value of calling the thunk), or Empty.
 ********************************************************************)
fun tail (Stream (_, thunk)) = thunk ()
  | tail _ = Empty
  ;
(*********************************************************************
 * Creates a new stream from an initial seed and a generator function
 * that is applied to the seed.
 ********************************************************************)
fun generate seed generator
  = Stream (seed, fn () => generate (generator seed) generator);
(*********************************************************************
 * Auxiliary tail recursive function
 * Takes and returns some number of elements from the stream.
 * Returns the list of elements and the remaining stream.
 ********************************************************************)
fun take'' 0 stream accum = (rev accum, stream)
  | take'' _ Empty  accum = (rev accum, Empty)
  | take'' n stream accum = take'' (n-1) (tail stream) ((head stream)::accum)
  (*********************************************************************
 * Takes and returns some number of elements from the stream.
 * Returns the list of elements and the remaining stream.
 ********************************************************************)
and take' n stream = take'' n stream []
(*********************************************************************
 * Takes and returns some number of elements from the stream.
 * Returns the list of elements.
 ********************************************************************)
and take n stream = #1 (take' n stream)
  ;
(*********************************************************************
 * Drops some number of elements from the stream.
 * Returns the remaining stream.
 ********************************************************************)
fun drop n stream = #2 (take' n stream)
  ;
(*********************************************************************
 * Maps a function over a stream.
 ********************************************************************)
fun smap _ Empty  = Empty
  | smap f stream = Stream (f (head stream), fn () => smap f (tail stream))
  ;
(*********************************************************************
 * Filters a stream. Returns only those elements that satisfy the
 * given predicate function.
 ********************************************************************)
fun filter _ Empty = Empty
  | filter pred stream =
      if (pred (head stream))
      then Stream (head stream, fn () => filter pred (tail stream))
      else filter pred (tail stream);
(*********************************************************************
 * Here are some useful functions and example streams.
 ********************************************************************)
fun isEven x = x mod 2 = 0;
fun isOdd  x = x mod 2 = 1;
fun inc  n x = x + n;
fun mult n x = x * n;
val ++ = inc 1;
val nats  = generate 1 (inc 1);
val evens = generate 0 (inc 2);
val odds  = generate 1 (inc 2);
(* prime numbers *)
fun sift stream k = filter (fn x => x mod k <> 0) stream;
fun sieve numbers =
  let val h = head numbers
      val t = tail numbers
  in Stream (h, fn () => sieve (sift t h))
  end;
val primes = sieve (drop 1 nats);

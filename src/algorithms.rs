/*!

Algorithms that don't seem to belong anywhere.

*/

/*
word add1(e,s) /* inserts e destructively into set s, kept in ascending address
             order */
word e,s;
{ word s1=s;
  if(s==NIL||e<hd[s])return(cons(e,s));
  if(e==hd[s])return(s); /* no duplicates! */
  while(tl[s1]!=NIL&&e>hd[tl[s1]])s1=tl[s1];
  if(tl[s1]==NIL)tl[s1]=cons(e,NIL);else
  if(e!=hd[tl[s1]])tl[s1]=cons(e,tl[s1]);
  return(s);
}
*/




#[cfg(test)]
mod tests {
  #[test]
  fn it_works() {
    assert_eq!(2 + 2, 4);
  }
}

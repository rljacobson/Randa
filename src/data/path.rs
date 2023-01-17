/*!
# NOTES on how static pathname resolution is achieved

The specification is that path names must always be resolved relative to the
file in which they are encountered.

*Definition:* the `prefix` of a path name is the initial segment up to but not
including the last occurrence of '/' (null if no '/' present).
Keep the `wd` constant during compilation.  Have a global char* prefix, initially
null.
1) Whenever you read a relative pathname(), insert 'prefix' on the front of it.
2) On entering a new level of insert, stack old prefix and prefix becomes that
   of new file name.  Done by calling adjust_prefix().
3) On quitting a level of insert, unstack old prefix.

*/



// Path manipulation functions that don't seem to belong anywhere.

/**
Returns a string containing the part of `path` up to but excluding the final '/', or the empty string if there is no
'/'. (Miranda's `setprefix()`.)

Before calling `dump_script` or `load_script`, must `setprefix()` to that of current path name of file being
dumped/loaded to get correct translation between internal path names (relative to dump script) and external path names.
*/
pub fn path_prefix(mut path: &String) -> String {
  let mut path = path.clone();

  // Todo: Make this platform independent by using path separator.
  if let Some(idx) = path.rfind('/') {
    // Take the part of the string up to the last '/'
    let _ = path.split_off(idx);
    path
  } else {
    "".to_string()
  }
}



/// Makes `path` correct relative to `prefix`. Must use when writing path names to dump. This function modifies `path`
/// in place.
pub fn make_relative_path(path: &mut String, prefix: &String) {
  // Todo: This algorithm is weird. It strips a prefix, presumably making an absolute path into a relative path,
  //       OR, if it doesn't have the prefix, it checks that it is in fact an absolute path...?
  //       Compare to block in `load_script`, factored out as `make_absolute_path` below.
  if Some(rest) = path.strip_prefix(prefix){
    // Replace the value in path with the prefix stripped.
    *path = rest.to_string();
  }
  else if path.starts_with('/') {
    // Do nothing
  }
  else{
    eprintln!("impossible event in make_relative_path");
    // Not possible because all relative path names in files were computed wrt current script.
  }
}


/// If `path` is relative, add `prefix` to `path` in-place.
pub fn make_absolute_path(path: &mut String, prefix: &String) {
  if !path.starts_with('/'){
    *path = prefix + path;
  }
}




#[cfg(test)]
mod tests {
  use super::{make_relative_path, path_prefix};

  #[test]
  fn make_relative_test() {
    let mut name = String::from("/home/rljacobson/Documents/code/miranda/main.rs");
    let prefix  =  String::from("/home/rljacobson/Documents/code/miranda/");

    let relative = make_relative_path(&mut name, &prefix);

    assert_eq!(relative, "main.rs");
  }

  #[test]
  fn path_prefix_text() {
    let mut name = String::from("/home/rljacobson/Documents/code/miranda/main.rs");
    let prefix  =  String::from("/home/rljacobson/Documents/code/miranda/");

    let relative = path_prefix(&name);

    assert_eq!(relative, prefix);
  }


}

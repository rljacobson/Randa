/*!



 */



fn token() -> Option<String> {
  extern crate std;
  use std::io::Read;

  let mut ch = std::io::stdin().bytes().next();
  let mut dicq = dicp;

  while ch == Some(b' ') || ch == Some(b'\t') {
    ch = std::io::stdin().bytes().next();
  }

  if ch == Some(b'~') {
    let mut h;
    dicq.push(ch.unwrap() as char);
    ch = std::io::stdin().bytes().next();
    while ch.is_alphanumeric() || ch == Some(b'-') || ch == Some(b'_') || ch == Some(b'.') {
      dicq.push(ch.unwrap() as char);
      ch = std::io::stdin().bytes().next();
    }
    dicq.push('\0');
    if let Some(home) = gethome(&dicp[1..]) {
      dicp = home.chars().collect();
      // FIX: dicq = dicp + strlen(dicp)
      dicq = dicp.len();
    }
  }
  #[cfg(SPACEINFILENAMES)]
  if ch != Some(b'"') && ch != Some(b'<') {
    while ch != Some(b' ') && ch != Some(b'\t') && ch != Some(EOF) {
      dicq.push(ch.unwrap() as char);
      if ch == Some(b'%') {
        if dicq[dicq.len() - 2] == '\\' {
          dicq[dicq.len() - 1] = '%';
        } else {
          dicq.pop();
          let current_script: Vec<char> = current_script.chars().collect();
          dicq.extend(current_script);
        }
      }
      ch = std::io::stdin().bytes().next();
    }
  }
  //#[cfg(SPACEINFILENAMES)]
  else {
    let closeq = if ch == Some(b'<') { b'>' } else { b'"' };
    dicq.push(ch.unwrap() as char);
    ch = std::io::stdin().bytes().next();
    while ch != Some(closeq) && ch != Some(b'\n') && ch != Some(EOF) {
      dicq.push(ch.unwrap() as char);
      ch = std::io::stdin().bytes().next();
    }
    if ch == Some(closeq) {
      dicq.push(ch.unwrap() as char);
      ch = std::io::stdin().bytes().next();
    }
  }
  dicq.push('\0');
  ovflocheck();
  while ch == Some(b' ') || ch == Some(b'\t') {
    ch = std::io::stdin().bytes().next();
  }
  std::io::stdin().bytes().unread(ch.unwrap());

  if *dicp == '\0' {
    None
  } else {
    Some(dicp.to_string())
  }
}

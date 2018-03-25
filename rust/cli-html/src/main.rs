extern crate html2text;
extern crate tabwriter;

use std::io::Write;
use tabwriter::TabWriter;

fn main() {
    let html = b"
    <ul>
      <li>Item one</li>
      <li>Item two</li>
      <li>Item three</li>
    </ul>";
    println!("### List:\n{}", html2text::from_read(&html[..], 20));

    let html = b"
    <table>
      <tr>
        <td>One</td>
        <td>Two</td>
        <td>Three</td>
      </tr>
      <tr>
        <td>1</td>
        <td>2.022323432</td>
        <td>333.3333333</td>
      </tr>
    </table>";
    println!("### html Table:\n{}", html2text::from_read(&html[..], 100));

    let mut tw = TabWriter::new(vec![]);
    write!(&mut tw, "\
| Bruce Springsteen\t| Born to Run
| Bob Seger\t| Night Moves
| Metallica\t| Black
| The Boss\t| Darkness on the Edge of Town").unwrap();
    tw.flush().unwrap();

    let written = String::from_utf8(tw.into_inner().unwrap()).unwrap();
    println!("### tab Table:\n{}", written);

    let html = b"
    <p>  This has     lots    of    spaces</p>
    <p>   This is a       new    p</p>
    ";
    println!("### Spaces:\n{}", html2text::from_read(&html[..], 20));
}

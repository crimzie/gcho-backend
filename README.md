<h3>Scala backend for an online GURPS toolkit.</h3>

<p>You can see a sample Charlist JSON structure in <code>example.json</code>.</p>
<p>/api/chars 
<br>GET — returns a list of charlists
<br>POST — validates the charlist in request and saves it to the database under new id, returns recalculated charlist
</p>
<p>/api/chars/default GET — returns a new blank charlist</p>
<p>/api/chars/:id
<br>GET — returns the charlist strored under this id
<br>PUT — validates the charlist in request and updates the charlist stored under this id with recieved one, then 
returns recalculated charlist
<br>PATCH — validates the update JSON in request and updates the charlist stored under this id, then returns 
recalculated charlist
<br>DELETE — removes the charlist stored under this id from the database</p>
<p>/api/chars/:id/pic
<br>GET — returns charlist portrait if it exists
<br>PUT — stores uploaded image as charlist portrair, overwriting any existing one</p>
<p>/api/[traits|skills|tecns|armors|weaps|items] 
<br>GET — returns the list of basic charlist components, [id, name]
<br>POST — validates the charlist component in request and saves it to the database under new id, returns recalculated 
charlist component</p>
<p>/api/traits?category=strings GET — returns a list of basic traits of any of 'category', [id, name]</p>
<p>/api/[traits|skills|tecns|armors|weaps|items]/default GET — returns default blank charlist component template</p>
<p>/api/[traits|skills|tecns|armors|weaps|items]/search?term=string GET — returns a list of basic charlist components 
with 'string' in name: [id, name]</p>
<p>/api/traits/search?category=strings;term=string GET — returns a list of basic traits of any of 'category' and with 
'string' in name: [id, name]</p>
<p>/api/[traits|skills|tecns|armors|weaps|items]/:id GET — returns a charlist component by id from the list of basics
</p>
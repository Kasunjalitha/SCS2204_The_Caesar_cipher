// Assignment - The Caesar cipher
// index number: 19000642

object CaesarCipher extends App{

	val textVal = "University Of Colombo School Of Computing";
	
	val encrypt = (key:Int, c:Char) => 
		if(c != ' ') (((c.toUpper.toInt - 65 + key)%26) + 65).toChar
		else ' ' 

	val decrypt = (key:Int, c:Char) => 
		if(c != ' ' &&  (c.toUpper.toInt - 65 - key) >= 0 ) (((c.toUpper.toInt - 65 - key)%26) + 65).toChar
		else if(c != ' ' &&  (c.toUpper.toInt - 65 - key) < 0 ) (((c.toUpper.toInt - 65 - key + 26)%26) + 65).toChar
		else ' '

	
	val cipher = (algo:(Int, Char) => Char, key:Int, msg:String) => msg.map(algo(key, _))

	val x = cipher(encrypt, 23, textVal)
	val y = cipher(decrypt, 23, x);

	println(x);
	println(y);	
}

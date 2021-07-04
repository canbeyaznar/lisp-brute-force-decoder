; *********************************************
; *  341 Programming Languages                *
; *  Fall 2019                                *
; *  Author: Yakup Genc                       *
; *********************************************

;;NAME: Can BEYAZNAR
;;NUMBER: 161044038

;; utility functions 
(load "include.lisp") ;; "c2i and "i2c"


(defun read-as-list (filename)
	; Reads a file containing one word per line and returns a list of words (each word is in turn a list of characters)."

	; Implement this function...

	(setq ListOfWords '())
	(setq ListOfChars '())

	(terpri)
	(terpri)
	(let ((in (open filename :if-does-not-exist nil)))
        (when in
            (loop for tempChar = (read-char in nil)
                while tempChar do
				(when (not (and (char>= tempChar #\a) (char<= tempChar #\z)))					

					(setq ListOfWords (append ListOfWords (list ListOfChars)))
					(setq ListOfChars '())
				)

				(if (and (char>= tempChar #\a) (char<= tempChar #\z))	
					;if it is true
					(setq ListOfChars (append ListOfChars (list tempChar)))				
				)

            )
				(setq ListOfWords (append ListOfWords (list ListOfChars)))
                (close in)
        )
    )
	(return-from read-as-list ListOfWords)

)

;; -----------------------------------------------------
;; HELPERS
;; *** PLACE YOUR HELPER FUNCTIONS BELOW ***

(defun spell-checker-0 (word dictionary)
	
	;; Sozlukteki tum kelimeler dolist'i kullanarak gez
	;; Eger bir kelimedeki tum harflar aranan kelime ile ayni ise
	;; true return et

	(setq lengthOfWord (length word))
	(dolist (eachDictionaryWord dictionary)
		(setq index 0)
		(setq correctLetterCounter 0)
		(if (= (length eachDictionaryWord) lengthOfWord )
			(progn 
			
				(loop 
					(when (= index (length eachDictionaryWord)) (return ))
					(if (char= (nth index eachDictionaryWord) (nth index word) ) (setq correctLetterCounter (+ correctLetterCounter 1)) )

					(setq index (+ index 1))
				)
				(if (= correctLetterCounter  lengthOfWord ) (return-from spell-checker-0 t) )
			)
		)	
	)

	(return-from spell-checker-0 NIL)
)

(defun spell-checker-1 (word)
 	;you should implement this function
)

(defun encode-paragraph (originalAlphabet originalParagraph)

	;kullanicidan alinan orjinal paragrafi sifreler
	;rastgele sifreli alfabe olusturarak orjinal paragrafin her bir harfine
	;bakarak cipher alfabedeki harfi yerine koyarak paragrafi sifreler.

	(setq CipherAlphabet (generate-cipher-alphabet originalAlphabet))
	(setq result '())
	(setq result (get-decoded-words-list originalParagraph originalAlphabet CipherAlphabet))

	(terpri)
	(print "Original Alphabet: ")
	(print originalAlphabet)
	(terpri)
	(print "Cipher Alphabet: ")
	(print CipherAlphabet)
	(terpri)
	(terpri)
	(return-from encode-paragraph result)
)

(defun random-from-range (start end)
	;iki sayi arasindan rasgele bir sayi return eder

	(setf *random-state* (make-random-state t))
	(+ start (random (+ 1 (- end start))))
)

(defun generate-cipher-alphabet(OriginalAlphabet)	
	
	;bu fonksiyon rastgele olusturulmus bir sifreli alfabe return olusturur

	(setq testerCipher '())
	(setq i 0)
	(loop 
		(when (> i 25) (return ))

		;0 ile 25 arasi rastgele bir sayi olustur
		;ardindan bu sayinin karakter karsiligini al
		(setq randomLetterIndex (random-from-range 0 25))
		(setq myRandomLetter (nth randomLetterIndex OriginalAlphabet) )
		(cond 
			;eger cipher alfabede yok ise bu harfi ekle ve index i arttir
			((not (find myRandomLetter testerCipher ))
				(setq testerCipher (append testerCipher(list myRandomLetter)) )
				(setq i (+ i 1))
			)
		)
	)			
	(return-from generate-cipher-alphabet testerCipher)

)

(defun generate-cipher-alphabet-1 (originalAlphabet oldCipherAlphabet constantLetters)
	
	;generate-cipher-alphabet-1 fonksiyonu Gen Decoder B icindir. generate-cipher-alphabet ten 
	;tek farki, Gen Decoder B icin kullanilan "e t a o i n" harflerinin cipher alfabede
	;sabit indexleri vardir. sifreli alfabede bu harflere karsilik gelen harflerin degismemesi
	;icin farkli bu fonksiyon yazilmistir.

	;Amaci "e t a o i n" ye karsilik gelen harfleri sabit tutarak, sifreli alfabede kalan indexlere
	;rastgele harf atayarak yeni bir sifreli alfabe olusturmaktir.
	
	(setq currentIndex 0)
	(loop 

		(if (= currentIndex (length originalAlphabet)) (return ))

		(setq randomLetterIndex (random-from-range 0 25))
		(setq myRandomLetter (nth randomLetterIndex originalAlphabet) )

		(if (equal (find (nth currentIndex oldCipherAlphabet) constantLetters) NIL) 
			(progn
			
				(if (and (equal (find myRandomLetter constantLetters ) NIL) 
						(equal (find myRandomLetter oldCipherAlphabet ) NIL))
						
					(setf (nth currentIndex oldCipherAlphabet) myRandomLetter )	
					(setq currentIndex (- currentIndex 1))				
				)
			
			)
		)

		(setq currentIndex (+ currentIndex 1))
	)

	(return-from generate-cipher-alphabet-1 oldCipherAlphabet )
)

(defun contains-cipher-alphabet (CreatedAlphabets currentAlphabet) 

	;Yaptigim programin algoritmasi birden fazla sifreli alfabe urettigi icin
	;onceden ayni alfabenin uretilip uretilmedigine bakiyorum
	;bu fonksiyonda kullanilmis tum alfabeler (CreatedAlphabets) ile 
	;guncel alfabeyi karsilastiriyor

	(setq listSize 26)
	(dolist (eachList CreatedAlphabets)
		(setq sameLetter 0)
		(setq tempList eachList)
		(setq index 0)
		(loop 
			(when (char= (nth index tempList) (nth index currentAlphabet))
				(setq sameLetter (+ sameLetter 1))
			)
			(When (= (- listSize index) index)
				(return )
			)
			(setq index (+ index 1))
		)
		(if (= listSize sameLetter) (return-from contains-cipher-alphabet t))
	)
	
	(return-from contains-cipher-alphabet NIL)
)

(defun get-decoded-words-list (paragraph OriginalAlphabet CipherAlphabet)

	;Bu fonksiyon sifreli alfabeyi kullanarak, sifreli paragrafi cozer ve return eder
	;sifreli paragrafin her bir harfine bakarak, sifreli alfabede karsiligina gelen harfi koyar

	(setq decodedParagraph paragraph)
	(setq indexOfCurrWord 0)
	(dolist (eachWord paragraph)

		(setq tempWord '())
		(setq indexOfCurrLett 0)

		(dolist (eachLetter eachWord)
			(setq indexLetter (position eachLetter OriginalAlphabet))
			(setq decodedLetter (nth indexLetter CipherAlphabet) )
			(setq tempWord (append tempWord (list decodedLetter)))
			(setq indexOfCurrLett (+ indexOfCurrLett 1))
		)
		(setf (nth indexOfCurrWord decodedParagraph) tempWord)
		(setq indexOfCurrWord (+ indexOfCurrWord 1))
	)
	(return-from get-decoded-words-list decodedParagraph)
)


(defun find-most-used-letters (used-letters count-of-letters)

	;Gen Decoder B icin
	;Bu fonksiyon bir paragrafta kullanilan tum harfleri sayilari ile birlikte alir
	;ve en cok kullanilan harften en az kullanilan harfe kadar siralar

	(setq i 0)
	(loop
		(if (= i (length used-letters))	(return ))
		(setq tempIndex i)	
		(setq j 0)
		(loop
			(if (= j (length used-letters))	(return ) )
			
			(if (> (nth tempIndex count-of-letters) (nth j count-of-letters) )
				(progn 

					(rotatef (nth tempIndex count-of-letters) (nth j count-of-letters))
					(rotatef (nth tempIndex used-letters) (nth j used-letters))
				)
			)
			(setq j (+ j 1))
		)
		(setq i (+ i 1))
	)
)

(defun get-most-used-six-letters (paragraph) 
	
	;Gen Decoder B icin
	;Bu fonksiyon bir paragrafta en cok kullanilan 6 harfi return eder

	(setq UsedLetters '())
	(setq countOfLetters '())
	(dolist (eachWord paragraph)

		(dolist (eachLetterPar eachWord)

			(if (eq (find eachLetterPar UsedLetters) NIL)

				(progn 
					(setq UsedLetters (append UsedLetters (list eachLetterPar)))
					(setq countOfLetters (append countOfLetters (list 1)))
				
				)

				(setf (nth (position eachLetterPar UsedLetters) countOfLetters) (+ (nth (position eachLetterPar UsedLetters) countOfLetters) 1))						
			)	
		)
	)
	;Harflerin kullanim oranini sirala
	(find-most-used-letters UsedLetters countOfLetters)

	;Eger elde edilen listenin uzunlugu 6 dan kucukse yani toplamda 6 dan daha az farkli harf kullanilmissa
	;listenin kendisini return eder
	;Eger degilse ilk 6 harfini bir listeye atarak return eder.
	(setq return-letters '())
	(if (< (length UsedLetters) 6 )
		(setq return-letters UsedLetters)
		(progn
			
			(setq size 0)
			(dolist (eachLetter UsedLetters) 
				(if (= size 6) (return ) )
				(setq return-letters (append return-letters (list eachLetter)))
				(setq size (+ size 1))
			)

		)
	)
	(return-from get-most-used-six-letters return-letters)

)

(defun all-permutations (list)

	;bir listenin icinde bulunan elemanlarin tum olasi siralanisini return eder
	(cond 
		((null list) NIL )
		((null (cdr list)) (list list))
		(t (loop for element in list
			append (mapcar (lambda (l) (cons element l))
							(all-permutations (remove element list)))))
	
	
	)

)


;; -----------------------------------------------------
;; DECODE FUNCTIONS

(defun Gen-Decoder-A (paragraph dictionary)

	;Bu fonksiyonda, rastgele alfabe olusturarak bize verilen metini sifreli alfabeye gore 
	;decode edip paragrafta bulunan kelimelerin hepsi ingilizce sozlukte bulunuyor ise decode edilen paragraf
	;return edilir.

	(setq OriginalAlphabet '() )
	(setq lower-a (char-int #\a))

	(dotimes (i 26)
		(setq OriginalAlphabet(append OriginalAlphabet (list (int-char (+ lower-a i))  )))
	)

	(setq CreatedAlphabets '()) ;this list will take all cipher alphabets that tried...
	(setq decodedWords '())

	(print "PLEASE WAIT")
	(loop 
		
		;Temporary cipher alfabe uret
		(setq tempChiperAlphabet (generate-cipher-alphabet OriginalAlphabet))
		(cond
			(
				;Eger bu alfabe CreatedAlphabets listesinde varsa yeniden uret
				(contains-cipher-alphabet CreatedAlphabets tempChiperAlphabet) 
			)
			(t 
				;eger yok ise bu alfabeyi kullanarak sifreli paragrafi decodeWords ile coz
				(setq decodedWords (get-decoded-words-list paragraph OriginalAlphabet tempChiperAlphabet))
				(setq correctWords 0)

				;Ardindan spell checker yardimi ile her bir kelimenin sozlukte olup olmadigini kontrol et
				(dolist (eachWord decodedWords)
					(if (spell-checker-0 eachWord dictionary) 
						(setq correctWords (+ correctWords 1))
					)	
				)

				;eger tum kelimeler var ise bu paragrafi return et
				(cond 
					(
						(= (list-length decodedWords) correctWords)
						(return-from Gen-Decoder-A decodedWords)
					)
				)
				(setq CreatedAlphabets (append CreatedAlphabets (list tempChiperAlphabet)))
			)
		)

		(print "-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-")
		(terpri)
		(print "Cipher Alphabet: ")
		(print tempChiperAlphabet)

		(terpri)
		(terpri)

		(print "Decoded Paragraph: ")
		(print decodedWords)

		(terpri)
		(terpri)

		(print "Count of finded words: ")
		(print correctWords)
		
		(terpri)
		(terpri)

		(print "-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-")
		(terpri)
	)
)

(defun Gen-Decoder-A-for-B-0 (paragraph mostUsedLett dictionary)

	;Bu fonksiyon en cok kullanilan 6 harfi alarak 
	;sifreli alfabede yerine koyar. ve kalan 20 harf icin rastgele harfler atayarak 
	;Gen decoder a da yapildigi gibi yeni bir sifreli alfabe olusturulur...


	(setq OriginalAlphabet '() )
	(setq lower-a (char-int #\a))

	(dotimes (i 26)
		(setq OriginalAlphabet(append OriginalAlphabet (list (int-char (+ lower-a i))  )))
	)

	(setq CreatedAlphabets '()) ;this list will take all cipher alphabets that tried...
	(setq decodedWords '())

	(setq mostUsedEnglishLetters '(#\e #\t #\a #\o #\i #\n))
	(print mostUsedEnglishLetters)
	(print mostUsedLett)
	
	(setq usedLetts '())
	(setq counter 0)
	(loop
		;Dongude bulunan counter a bagli parametre 
		;Eger 100 defa ust uste CreatedAlphabets listesinde bulunan bir alfabe uretilirse
		;bu donguden cikmak icin kullanilir. Ve ardindan Gen Decoder B 720 olasilik icerisinden listedin
		;diger elemanini denemek icin bu fonksiyonu yeniden cagirir

		(if (= counter 100) (return-from Gen-Decoder-A-for-B-0 NIL) )

		(setq cipherAlphabet '())

		;sifreli alfabede oncelikle her birine -1 ata
		(dotimes (i 26)
			(setq cipherAlphabet (append cipherAlphabet (list -1)))
		)


		;ardindan bizim bildigimiz en cok kullanilan 6 harfi bu alfabeye ekle
		(setq index 0)
		(dolist (eachLetter mostUsedLett)
			(setf (nth (c2i (nth index mostUsedEnglishLetters)) cipherAlphabet) eachLetter)
			(setq usedLetts (append usedLetts (list eachLetter)))
			(setq index (+ index 1))
		)

		;Ardindan bu 6 harf degismeyecek sekilde kalan 20 harf icin bir sifreli alfabe olustur
		(setq cipherAlphabet (generate-cipher-alphabet-1 originalAlphabet cipherAlphabet mostUsedLett)  )
		(cond
			(
				;Bu alfabe CreatedAlphabets listesinde var ise yeniden uret
				(contains-cipher-alphabet CreatedAlphabets cipherAlphabet)
				(setq counter (+ counter 1))
			)
			(t 
				(setq decodedWords (get-decoded-words-list paragraph OriginalAlphabet cipherAlphabet))
				(setq correctWords 0)
				(setq counter 0)

				(dolist (eachWord decodedWords)
					(if (spell-checker-0 eachWord dictionary) 
						(setq correctWords (+ correctWords 1))
					)	
				)

				(cond 
					(
						(= (list-length decodedWords) correctWords)
						(return-from Gen-Decoder-A-for-B-0 decodedWords)
					)
				)
				(setq CreatedAlphabets (append CreatedAlphabets (list cipherAlphabet)))
				
			)
		)

		(print "-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-")
		(terpri)
		(print "Cipher Alphabet: ")
		(print cipherAlphabet)

		(terpri)
		(terpri)

		(print "Decoded Paragraph: ")
		(print decodedWords)

		(terpri)
		(terpri)

		(print "Count of finded words: ")
		(print correctWords)
		
		(terpri)
		(terpri)

		(print "-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-o-")
		(terpri)
	)


	(return-from Gen-Decoder-A-for-B-0 NIL)

)


(defun Gen-Decoder-B-0 (paragraph dictionary)
  	
	;Bu fonksiyon, paragrafta en cok kullanilan 6 harfi alip,
	;"e t a o i n" harflerinin olusturdugu 6 faktoriyellik (6!) permutasyonda
	;her bir olasiligi karsilik olarak koyar. ornek vermek gerekirse, 
	;En cok kullanilan harfler listemiz "a b c d e f" olsun.
	;artik alfabede e karsisina a harfi gelecektir. (e -> a, t -> b, a->c gibi...)

	;ayrica "e t a o i n" harflerinin olusturabilecegi 720 farkli dizilis oldugu icin
	;tum hepsi denenmek zorundadir.
	;Fonksiyon en sik kullanilan 6 harfi bulduktan sonra kalan 20 harfi bulmak icin
	;Brute force yontemi yapilir yani Gen Decoder A ya gonderilir...


	(setq mostUsedLetters (get-most-used-six-letters paragraph) )
	(setq allPermutations (all-permutations mostUsedLetters))

	
	(dolist (eachProbability allPermutations)
		;Her bir olasilik icin Gen-Decoder-A-for-B-0 fonksiyonunu calistir
		(setq result (Gen-Decoder-A-for-B-0 paragraph eachProbability dictionary))
		(if (not (eq result NIL))
			(return-from Gen-Decoder-B-0 result)
		)
	)

	(return-from Gen-Decoder-B-0 NIL)
	
)


(defun Gen-Decoder-B-1 (paragraph)
  	;you should implement this function
)

(defun Code-Breaker (document decoder)
	
  	(return-from Code-Breaker decoder)
)

;; -----------------------------------------------------
;; Test code...

(defun test_on_test_data ()
	(print "....................................................")
	(print "Testing ....")
	(print "....................................................")
	(terpri)

	(let ((doc (read-as-list "document1.txt")))

		(print doc)
		(let ((Dictionary (read-as-list  "dictionary1.txt")))
			;(print (Code-Breaker doc (Gen-Decoder-A doc Dictionary)))
			(print (Code-Breaker doc (Gen-Decoder-B-0 doc Dictionary)))
		)
				
		
	)
)


;; test code...
(test_on_test_data)
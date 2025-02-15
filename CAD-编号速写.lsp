;参数初始化
(defun chushihua()
  ;(setvar "cmdecho" 0)
  (setq bhlx 0)
  (write-line "编号类型:0")
  (setq bh0 0)
  (write-line "编号初值:1")
  (setq bh1 64)
  (setq bh2 0)
  (setq bh3 64)
  (setq yx "N")
  (write-line "引线:N")
  (setq bklx "N")
  (write-line "边框:N")
  (setq r 10)
  (setq zidong_r t)
  (write-line "边框圆半径:自动")
  (setq th 10)
  (write-line "文字高度:10")
  (setq ta 0)
  (write-line "文字旋转角度:0")
  (setq jl 1)
  (write-line "编号到引线距离:1")
  (setq jiantou 2.5)
  (write-line "箭头大小:2.5")
  (setq chang 5)
  (setq zidong_chang t)
  (write-line "水平段引线长度:自动")
  (setq qianzhui2 "")
  (write-line "自然数前缀:无")
  (setq houzhui2 "")
  (write-line "自然数后缀:无")
  (setq qianzhui3 "")
  (write-line "字母前缀:无")
  (setq houzhui3 "")
  (write-line "字母后缀:无")
  (setq pianyi 7)
  (write-line "边框自动处理时,文字到边框的偏移系数:7")
 (write-line "程式:编号速写，命令:ksbh")

  
  
)


(chushihua)

;主函数*********************************************************************


(defun c:ksbh(/ o ob)
  (defun er(str)
    (setvar "osmode" dx)    
    (princ "\nSB已取消")    
  )
  (setq erod *error* *error* er)


  (if (= yx "N")
    (progn
      (initget "C Y K W H T")
      (setq ob (getpoint "\n指定编号位置[编号初值(C)/引线(Y)/边框(K)/文字(W)/还原为默认设置(H)/退出(T)]:<编号之前请先将当前文字样式的文字高度项设为0>"))
      (if (or (= ob "C") (= ob "Y") (= ob "T") (= ob "K") (= ob "W") (= ob "H") (= ob nil))
	(progn
	  (if (= ob "C") (chuzhi))           
          (if (= ob "Y") (yinxian))
	  (if (= ob "K") (bkshezhi))
	  (if (= ob "W") (wenzi))
	  (if (= ob "H") (chushihua))
	  (if (= ob nil) (gaodu0))


	  (if (= ob "T") (exit))
	)
        (progn
	  (huitu o ob)
	)
      )
    )
    (progn
      (initget "C Y K W H T")
      (setq o (getpoint "\n指定标注位置[编号初值(C)/引线(Y)/边框(K)/文字(W)/还原为默认设置(H)/退出(T)]:<编号之前请先将当前文字样式的文字高度项设为0>"))
      (if (or (= o "C") (= o "Y") (= o "T") (= o "K") (= o "W") (= o "H") (= o nil))
	(progn
	  (if (= o "C") (chuzhi))           
          (if (= o "Y") (yinxian))
	  (if (= o "K") (bkshezhi))
	  (if (= o "W") (wenzi))
	  (if (= o "H") (chushihua))
	  (if (= o nil) (gaodu0))


	  (if (= o "T") (exit))
	)
        (progn
          (initget 1)
	  (setq ob (getpoint o "\n指定编号位置:"))	  
	  (huitu o ob)
	)
      )
    )    
  )

)


;绘制过程*********************************************************************************

(defun huitu(o ob / dian1 dian2 dianx diany jiantou0 chang0)
  (graphscr)

  (setq jiantou0 jiantou)
  (command "text" '(0 0) th ta "编号速写" "erase" (entlast) "")
  

  
  
  ;编号升序
  ;0 (1 2 3...) 1 (a b c.../A B C...) 2 (a1 a2.../B1 B2...) 3 (aa ab ac.../Ma Mb Mc...)
  (if (= bhlx 0)
    (progn
      (setq bh0 (+ bh0 1))
      (setq bh (itoa bh0))
    )
  )
  (if (= bhlx 1)
    (progn
      (setq bh1 (+ 1 bh1))
      (if (> bh1 122) (setq bh1 97))
      (if (and (> bh1 90) (< bh1 97)) (setq bh1 65))
      (if (< bh1 65) (setq bh1 65))
      (if (and (< bh1 97) (> bh1 90)) (setq bh1 97))
      (setq bh (chr bh1))
    )
  )

  (if (= bhlx 2)
    (setq bh (strcat qianzhui2 (itoa (setq  bh2 (+ 1 bh2))) houzhui2))
  )

  (if (= bhlx 3)
    (progn
      (setq bh3 (+ 1 bh3))
      (if (> bh3 122) (setq bh3 97))
      (if (and (> bh3 90) (< bh3 97)) (setq bh3 65))
      (if (< bh3 65) (setq bh3 65))
      (if (and (< bh3 97) (> bh3 90)) (setq bh3 97))
      (setq bh (strcat qianzhui3 (chr bh3) houzhui3))
    )
  
  )

  (if (= bklx "Y")       ;边框半径是否自动处理及r0值
    (progn
      (if (= zidong_r t)
	(setq r0 (/ (tdaxiao 3) 2))
	(setq r0 r)
      )
    )
    (setq r0 (/ (tdaxiao 2) 2))
  )
  (if (= bklx "J")       ;边框为矩形时r0值
    (if (= zidong_j t)
      (setq r0 (/ (tdaxiao 2) 2))
      (setq r0 (/ jgao 2))
    )
  )
  
  (if (= zidong_chang t) ;引线长度是否自动处理及chang0值
    (progn
      (if (= bklx "Y")
	(setq chang0 (* r0 2))
	(progn
          (if (= bklx "J")
	    (if (= zidong_j t)
	      (setq chang0 (tdaxiao 1))
	      (setq chang0 jkuan)	   
	    )
	    (if (= bklx "W")
	      (setq chang0 (tdaxiao 1))
	    )
	  )
	) 
	
      )
    )
    (setq chang0 chang) 
  )

  ;对象捕捉
  (setq dx (getvar "osmode"))
  (setvar "osmode" 0)  


  
  
  (biankuangy r0 ob bh)
  (if (and (/= yx "N") (/= yx "n"))
    (progn
      (if (>= (car o) (car ob))
        (progn 
          (setq dian1 (list(+ (car ob) (/ chang0 2)) (- (cadr ob) r0 jl)))
          (setq dian2 (list(- (car ob) (/ chang0 2)) (- (cadr ob) r0 jl)))
          (if (> jiantou0 (distance o dian1)) (setq jiantou0 0))
	  (setq dianx (+ (car o) (* (/ jiantou0 (distance o dian1)) (- (car dian1) (car o)))))
	  (setq diany (+ (cadr o) (* (/ jiantou0 (distance o dian1)) (- (cadr dian1) (cadr o)))))
	  (command "pline" o "w" 0 (/ jiantou0 3) (list dianx diany) "w" 0 0 dian1 dian2 "")

	  
        )
	(progn
          (setq dian1 (list(- (car ob) (/ chang0 2)) (- (cadr ob) r0 jl)))
	  (setq dian2 (list(+ (car ob) (/ chang0 2)) (- (cadr ob) r0 jl)))
	  (if (> jiantou0 (distance o dian1)) (setq jiantou0 0))
          (setq dianx (+ (car o) (* (/ jiantou0 (distance o dian1)) (- (car dian1) (car o)))))
	  (setq diany (+ (cadr o) (* (/ jiantou0 (distance o dian1)) (- (cadr dian1) (cadr o)))))
	  (command "pline" o "w" 0 (/ jiantou0 3) (list dianx diany) "w" 0 0 dian1 dian2 "")

	  
        )
      )
    )
  )
  ;对象捕捉
  (setvar "osmode" dx)
  (setq *error* erod erod nil)

(eval bh)
  
)



;参数设置********************************************************************************

;初值设置
(defun chuzhi(/ qianzhui20 houzhui20 qianzhui30 houzhui30)
      
      
      (initget 1 "0 1 2 3")
      (setq bhlx (atoi (getkword "选择一种编号类型:\n<0>  自然数序列(1 2 3...)  <1>  字母序列(a b c.../A B C...)  <2>  带前/后缀自然数序列(a1 a2.../B1 B2.../(1)...)  <3>  带前/后缀字母序列(aa ab ac.../Ma Mb Mc.../(a)...)  选择其中一种0/1/2/3:")))
      (if (= bhlx 0)
        (progn
          (setq bh0 (getint "\n输入起始数值:<1>"))
	  (if (= bh0 nil) (setq bh0 1))
	  (setq bh0 (1- bh0))
	)
      )
      (if (= bhlx 1)
        (progn	  
	  (setq bh1 (1- (ascii (getstring "\n输入起始字母:<A>"))))
	)
      )
      (if (= bhlx 2)
        (progn
	  (setq qianzhui20 (getstring (strcat "输入前缀:<" qianzhui2 ">")))
	  (if (/= qianzhui20 "") (setq qianzhui2 qianzhui20))
	  (setq bh2 (getint "\n输入起始数值:<1>"))
	  (if (= bh2 nil) (setq bh2 1))
	  (setq bh2 (1- bh2))
	  (setq houzhui20 (getstring (strcat "输入后缀:<" houzhui2 ">")))
	  (if (/= houzhui20 "") (setq houzhui2 houzhui20))
	)
      )
      (if (= bhlx 3)
        (progn
	  (setq qianzhui30 (getstring (strcat "输入前缀:<" qianzhui3 ">")))
	  (if (/= qianzhui30 "") (setq qianzhui3 qianzhui30))
	  (setq bh3 (1- (ascii (getstring "\n输入起始字母:<A>"))))
	  (setq houzhui30 (getstring (strcat "输入后缀:<" houzhui3 ">")))
	  (if (/= houzhui30 "") (setq houzhui3 houzhui30))
	)
      )

    

  (c:sb)
)

;引线设置
(defun yinxian(/ yx0 yxf jl0 chang0 jiantou0)
  (initget "Y N")
  (if (= yx "Y") (setq yxf "N") (setq yxf "Y"))
  (setq yx0 (getkword (strcat "\n" "编号是否带引线？<Y>/<N>:<" yxf ">")))
  (if (= yx0 nil) (setq yx yxf) (setq yx yx0))
  (if (= yx "Y")
    (progn
      (setq jl0 (getdist (strcat "输入编号到引线的距离:<" (rtos jl) ">")))
      (if (/= jl0 nil) (setq jl jl0))
      (setq chang0 (getdist "输入水平段引线的长度:<自动>"))
      (if (/= chang0 nil) (setq chang chang0 zidong_chang nil) (setq zidong_chang t))
      (setq jiantou0 (getdist (strcat "输入箭头大小:<" (rtos jiantou) ">")))
      (if (/= jiantou0 nil) (setq jiantou jiantou0))


      
    )
  )
  (c:sb)
)

;边框设置
(defun bkshezhi(/ r0)
  (initget 1 "Y W J")
  (setq bklx (getkword "\n选择边框类型[圆(Y)/矩形(J)/无(W)]:"))
  
  (if (= bklx "Y")
    (progn
      
      (setq r0 (getdist (strcat "输入圆半径:<" "自动" ">")))
      (if (/= r0 nil) (setq r r0 zidong_r nil) (setq zidong_r t))
    )
  )
  (if (= bklx "J")
    (progn
      (setq jkuan (getdist "\n输入矩形宽:<自动>"))
      (if (= jkuan nil)
	(setq zidong_j t)
	(progn
	  (setq zidong_j nil)
	  (setq jgao (getdist "\n输入矩形高度:"))
        ) 
      )
    )
  )
  

  
  
  
  

(c:sb)
)
;文字设置
(defun wenzi(/ th0 ta0)
(setq th0 (getdist (strcat "输入文字高度:<" (rtos th) ">")))
  (if (/= th0 nil)
    (progn
      (setq th th0)
      
    )
    
  )
      
  
  (setq ta0 (getangle (strcat "输入文字角度:<" (rtos ta) ">")))
  (if (/= ta0 nil)
    (progn
      (setq ta ta0)
      (setq ta (* (/ ta pi) 180))
    )
  )

(c:sb)
)  

;绘边框  
(defun biankuangy(banjing yuanxin bianhao)
  (if (= bklx "Y")
    (command "circle" yuanxin banjing)
  )
  (if (= bklx "J")
    (progn
      (if (= zidong_j t)
	(setq jkuan0 (tdaxiao 1) jgao0 (tdaxiao 2))
	(setq jkuan0 jkuan jgao0 jgao)
      )
      (command "pline")
      (command (list (- (car yuanxin) (/ jkuan0 2)) (- (cadr yuanxin) (/ jgao0 2))))
      (command "w" 0 0)
      (command (list (+ (car yuanxin) (/ jkuan0 2)) (- (cadr yuanxin) (/ jgao0 2))))
      (command (list (+ (car yuanxin) (/ jkuan0 2)) (+ (cadr yuanxin) (/ jgao0 2))))
      (command (list (- (car yuanxin) (/ jkuan0 2)) (+ (cadr yuanxin) (/ jgao0 2))))
      (command "c")
    )
  )



  
  (command "text" "J" "MC" yuanxin th ta bianhao)

)  
;将当前文字样式的文字高度项设置为0
(defun gaodu0() 
  (command "style" "" "" 0 "" "" "" "" "")
  (c:sb)
)


;编号文字宽与高
(defun tdaxiao(x / a b c)  ;x=t则b=宽.x=nil则b=高
  (setq a (textbox (list (cons 1 bh))))
  (setq c (/ (- (cadar a) (cadadr a)) pianyi))
  (if (= x 1)
    (setq b (- (caadr a) (caar a) c))
    (if (= x 2)
      (setq b (- (cadadr a) (cadar a) c))
      (setq b (- (distance (car a) (cadr a)) c))
    )
  )
  (eval b)
)
  


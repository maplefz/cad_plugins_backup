
;�����������
;================================================================
(defun c:zzz (/ $fontlist$ $sydzt$ addlist av:changefontsdcl dclid desetpopx-0 desetpopx-1 fname getpop key setcusfontstype setpop1-0 setpop1-1 setpop2-0 setpop2-1)
  (defun av:changefontsdcl (fname / dcls fn)
    (setq dcls
			(list
				"fonts:dialog {"
				"    initial_focus = \"13\" ;"
				"    children_alignment = left ;"
				"    children_fixed_width = true ;"
				"    key = \"k00\" ;"
				"    label = \"CAD���������滻-www.cadzxw.com\" ;"
				"    :row {"
				"        :button {"
				"            key = \"11\" ;"
				"            label = \"1.�������(&K)\" ;"
				"        }"
				"        :text {"
				"            label = \"��ȱ�������Ϊ�������������\" ;"
				"        }"
				"    }"
				"    :row {"
				"        :button {"
				"            key = \"12\" ;"
				"            label = \"2.��������(&F)\" ;"
				"        }"
				"        :text {"
				"            label = \"����������������Ͱ������\" ;"
				"        }"
				"    }"
				"    :row {"
				"        :button {"
				"            key = \"13\" ;"
				"            label = \"3.��������(&Z)\" ;"
				"        }"
				"        :text {"
				"            label = \"ȫ�����͵���Ϊtssdeng��tssdchn\" ;"
				"        }"
				"    }"
				"    :row {"
				"        :button {"
				"            key = \"14\" ;"
				"            label = \"4.��������(&G)\" ;"
				"        }"
				"        :text {"
				"            label = \"ȫ�����͵���Ϊgbenor��gbcbig\" ;"
				"        }"
				"    }"
				"    :row {"
				"        :button {"
				"            key = \"15\" ;"
				"            label = \"5.��������(&S)\" ;"
				"        }"
				"        :text {"
				"            label = \"ȫ�����͵���Ϊ��������\" ;"
				"        }"
				"    }"
				"    :row {"
				"        :button {"
				"            key = \"16\" ;"
				"            label = \"6.�ֲ�����(&R)\" ;"
				"        }"
				"        :text {"
				"            label = \"�������������޷�ˢ�µ�����\" ;"
				"        }"
				"    }"
				"    spacer;"
				"    :boxed_column {"
				"        fixed_width = true ;"
				"        label = \"-7.�Զ�������\" ;"
				"        :row {"
				"            :column {"
				"                :row {"
				"                    :text {"
				"                        fixed_width = true ;"
				"                        key = \"pop1-t\" ;"
				"                        label = \"����1:\" ;"
				"                        width = 12 ;"
				"                    }"
				"                    :popup_list {"
				"                        fixed_width = true ;"
				"                        key = \"pop1\" ;"
				"                        width = 17 ;"
				"                    }"
				"                }"
				"                :row {"
				"                    :text {"
				"                        fixed_width = true ;"
				"                        key = \"pop2-t\" ;"
				"                        label = \"����2:\" ;"
				"                        width = 12 ;"
				"                    }"
				"                    :popup_list {"
				"                        fixed_width = true ;"
				"                        key = \"pop2\" ;"
				"                        width = 17 ;"
				"                    }"
				"                }"
				"            }"
				"            :column {"
				"                fixed_width = true ;"
				"                width = 10 ;"
				"                spacer;"
				"                :toggle {"
				"                    key = \"sydzt\" ;"
				"                    label = \"ʹ�ô�����\" ;"
				"                }"
				"                :button {"
				"                    key = \"ok\" ;"
				"                    label = \"�޸�\" ;"
				"                }"
				"            }"
				"        }"
				"    }"
				"    :button {"
				"        is_cancel = true ;"
				"        is_enabled = false ;"
				"        label = \"��ʾ������������������ͣ�ͼ��������Ϊ˳����\" ;"
				"    }"
				"    spacer;"
				"}"
	    )
    )
    (if	(setq fn (open fname "w"))
      (progn
				(foreach dcl dcls (write-line dcl fn))
				(close fn)
				fname
      )
    )
  )
	
	;AutoCAD-86 bigfont 1.0������
	;AutoCAD-86 unifont 1.0��������
	;AutoCAD-86 shapes 1.0���ļ�
	;ȡ���ض����������б�
	(defun $fontlist$ (txt / $fonttype$ delsame dir1 dirlist fontlist fonts getfontlist)
		;�ж���������
		(defun $fonttype$ (fn txt / cnt dv fh fonttype inp)
			(setq cnt 22 dv  "")
			(if	(setq fh (open fn "r"))
				(progn
					(while
						(and
							(> (setq cnt (1- cnt)) 0)
							(setq inp (read-char fh))
							(> inp 0)
						)
						(setq dv (strcat dv (chr inp)))
					)
					(close fh)
					(and dv (setq fonttype (WCMATCH (strcase dv t) txt)))
				)
			)
			fonttype
		)
		;�õ��ļ����ڶ�Ӧ�����б�
		(defun getfontlist(dir txt / files fontlist)
			(setq files (vl-directory-files dir "*.shx"))
			(setq fontlist
				(vl-remove-if-not
					(function
						(lambda(a)($fonttype$ (strcat dir "\\" a) txt))
					)
					files
				)
			)
		)
		;�����б����ظ�����
		(defun delsame(biao)
			(if biao
				(setq biao (cons (car biao) (delsame (vl-remove (car biao) (cdr biao)))))
			)
			biao
		)
		;��ѯ����ļ����������б�
		(and *fstl_dir* (setq dir1 (findfile (strcat *fstl_dir* "\\support"))))
		(setq dirlist (list dir1 (findfile "fonts")))
		(foreach dir dirlist
			(setq fonts (getfontlist dir txt))
			(setq fontlist (append fontlist fonts))
		)
		(setq fontlist (delsame fontlist));�б�ȥ��
		;(setq fontlist (acad_strlsort fontlist));�б�����
	)
	;�Ի���ؼ����
  (defun AddList (key lst)
    (IF	(AND key lst)
      (PROGN
				(if (= (type lst) 'str)(setq lst (list lst)))
				(start_list key)
				(foreach x lst (AND X (= (type x) 'str) (add_list x)))
				(end_list)
      )
      (PROGN
				(start_list key)
				(end_list)
      )
    )
    lst
  )
	;д���б�ļ��д��
	;(defun setpop(fonts)
	;	(start_list "pop1")
	;	(mapcar 'add_list fonts)
	;	(end_list)
	;)
	;���ð�ť����
	(defun setpop1-1(/ shx shx1 shx2)
		(setq shx1 ($fontlist$ "*unifont*"));��������
		(setq shx2 ($fontlist$ "*shapes*"));������
		(setq shx (acad_strlsort(append shx1 shx2)))
		(mode_tile "pop1" 0)
		(AddList "pop1" shx)
		(set_tile "pop1-t" "SHX����(X):")
		shx
	)
	(defun setpop2-1(/ bigs)
		(setq bigs ($fontlist$ "*bigfont*"));������
		(mode_tile "pop2" 0)
		(AddList "pop2" bigs)
		(set_tile "pop2-t" "������(B):")
		bigs
	)
	(defun setpop1-0(/ ttfs)
		(set_tile "pop1-t" "������(F):")
		(setq ttfs (list "΢���ź�" "����" "����" "����" "����" "������"))
		(AddList "pop1" ttfs)
		ttfs
	)
	(defun setpop2-0()
		(set_tile "pop2-t" "������ʽ(Y):")
		(mode_tile "pop2" 1)
		(AddList "pop2" nil)
	)
	(defun desetpopx-1()(setq unis (setpop1-1))(setq bigs (setpop2-1)))
	(defun desetpopx-0()(setq ttfs (setpop1-0))(setpop2-0))
  (defun $sydzt$ (/ pick)
    (setq pick (get_tile "sydzt"))
    (if(= pick "1")(desetpopx-1)(desetpopx-0))
  )
	;ȡ���б�����
	(defun getpop(pop fontlist / font pick)
		(setq pick (atoi(get_tile pop)))
    (if fontlist (setq font (nth pick fontlist)))
    font
	)
	;�����Զ���������ʽ
	(defun setcusfontstype(/ pop1-0 pop1-1 pop2-1 sydzt)
		(setq pop1-1 (getpop "pop1" unis))
		(setq pop1-0 (getpop "pop1" ttfs))
		(setq pop2-1 (getpop "pop2" bigs))
		(setq sydzt (get_tile "sydzt"))
		(cond
			((= sydzt "0")(av:fontstottf pop1-0))
			((= sydzt "1")(av:fontstoshx pop1-1 pop2-1))
			(t nil)
		)
	)
	;��ʼ����
	(vl-load-com)
  (setvar "cmdecho" 0)
	;(setq *acad* (vlax-get-acad-object))
	;(setq *doc* (vla-get-ActiveDocument *acad*))
  (and
		(setq fname (vl-filename-mktemp nil nil ".dcl"))
		(av:changefontsdcl fname)
		;(setq fname (findfile "fontsdcl.dcl"))
		(setq dclid (load_dialog fname))
  )
	(and dclid (new_dialog "fonts" dclid ""))
	(action_tile "11" "(done_dialog 11)")
	(action_tile "12" "(done_dialog 12)")
	(action_tile "13" "(done_dialog 13)")
	(action_tile "14" "(done_dialog 14)")
	(action_tile "15" "(done_dialog 15)")
	(action_tile "16" "(c:regenlocal)(done_dialog 0)")
	(action_tile "sydzt" "($sydzt$)")
	(action_tile "ok" "(setcusfontstype)(done_dialog 1)")
	(desetpopx-0);Ĭ�ϲ�ʹ�ô�����ʱ����ʾ��ʽ
	(setq key (start_dialog))
	(unload_dialog dclid)
	(cond
		((= key 11) (av:0totc "tssdeng" "tssdchn" "����"))
		((= key 12) (av:fontstofh "tssdeng" "tssdchn"))
		((= key 13) (av:fontstoshx "tssdeng" "tssdchn"))
		((= key 14) (av:fontstoshx "gbenor.shx" "gbcbig.shx"))
		((= key 15) (av:fontstottf "����"))
		((= key 1) nil)
		(t (setq key nil))
	)
	(if key (repeat *n-regen* (vla-regen *doc* 0)))
  (vl-file-delete fname)
  (setvar "cmdecho" 1)
  (princ)
)

(vl-load-com)
(setq *acad* (vlax-get-acad-object))
(setq *doc* (vla-get-ActiveDocument *acad*))
(setq *n-regen* 1)
;================================================================

(defun av:toshx (x shxx shxb)
	(vla-put-fontfile x shxx)
	(vla-put-bigfontfile x shxb)
)
(defun av:tottf (x ttf)
	(vla-setfont x ttf b c d e)
)
(defun c:fontstofh()
	(setvar "cmdecho" 0)
	(av:fontstofh "tssdeng" "tssdchn")
	(repeat *n-regen* (vla-regen *doc* 0))
  (setvar "cmdecho" 1)
  (princ)
)
(defun av:fontstofh(shxx shxb / xn)
	(vlax-for x (vla-get-textstyles *doc*)
		(vla-getfont x 'a 'b 'c 'd 'e)
		(setq xn (vla-get-name x))
		(cond
			((wcmatch xn "*����*") (av:tottf x "����"))
			((wcmatch xn "*����*") (av:tottf x "����"))
			((wcmatch xn "*����*") (av:tottf x "����"))
			((wcmatch xn "*����*") (av:tottf x "����"))
			(t (av:toshx x "tssdeng.shx" "tssdchn.shx"))
		)
  )
	(princ (strcat "\n>>>�����滻Ϊshx����ͷ�������"))
)
(defun c:fontstofz()
	(setvar "cmdecho" 0)
	(av:fontstoshx "tssdeng" "tssdchn")
	(repeat *n-regen* (vla-regen *doc* 0))
  (setvar "cmdecho" 1)
  (princ)
)
(defun c:fontstogb()
	(setvar "cmdecho" 0)
	(av:fontstoshx "gbenor.shx" "gbcbig.shx")
	(repeat *n-regen* (vla-regen *doc* 0))
  (setvar "cmdecho" 1)
  (princ)
)
(defun av:fontstoshx(shxx shxb)
	(vlax-for x (vla-get-textstyles *doc*)
		(av:toshx x shxx shxb)
		;(princ (entmod(entget(tblobjname "style" (vla-get-name x)))))
  )
	(princ (strcat "\n>>>�����滻Ϊ" shxx "��" shxb))
)
(defun c:fontstofs()
	(setvar "cmdecho" 0)
	(av:fontstottf "����")
	(repeat *n-regen* (vla-regen *doc* 0))
  (setvar "cmdecho" 1)
  (princ)
)
(defun av:fontstottf(ttf)
	(vlax-for x (vla-get-textstyles *doc*)
		(vla-getfont x 'a 'b 'c 'd 'e)
		(av:tottf x ttf)
  )
	(princ (strcat "\n>>>�����滻Ϊ" ttf))
)
(defun av:0totc	(shxx shxb ttf / big err shx)
	(vlax-for x	(vla-get-TextStyles *doc*)
		(vla-getfont x 'a 'b 'c 'd 'e)
		(if (= a "")
			(progn
				(cond
					((findfile (setq shx (strcat (vla-get-fontfile x) ".shx")))
						(vla-put-fontfile x shx)
					)
					((findfile (setq shx (vla-get-fontfile x)))
						(vla-put-fontfile x shx)
					)
					(t (vla-put-fontfile x shxx))
				)
				(cond
					((findfile (setq big (strcat (vla-get-bigfontfile x) ".shx")))
						(vla-put-bigfontfile x big)
					)
					((findfile (setq big (vla-get-bigfontfile x)))
						(vla-put-bigfontfile x big)
					)
					(t (vla-put-bigfontfile x shxb))
				)
			)
			(progn
				(setq err	(vl-catch-all-apply 'vla-setfont (list x a b c d e)))
				(if (vl-catch-all-error-p err) (vla-setfont x ttf b c d e))
			)
		)
	)
	(princ(strcat "\n>>>�����ͷֱ��滻Ϊ" shxx "��" shxb "��" ttf))
)

;================================================================

;�ֲ�ˢ��������
(defun c:r()
	(princ "-->�ֲ�������\n")
	(c:regenlocal)
)
(defun c:regenlocal (/ $screen atio ce ch ch2 diftime hh hh2 immtime p1 p2 re1 re2 ss)
	(setvar "cmdecho" 0)
	(setq $screen (getvar "SCREENSIZE")) 
	(setq ch (getvar "viewsize")) 
	(setq ch2 (/ ch 2)) 
	(setq ce (getvar "viewctr")) 
	(setq atio (/ (car $screen) (cadr $screen))) 
	(setq hh (* atio ch)) (setq hh2 (/ hh 2))
	(setq p1 (polar (polar ce 0 hh2) (* 1.5 pi) ch2))
	(setq p2 (polar (polar ce pi hh2) (* 0.5 pi) ch2))
	(setq ss (ssget "C" p1 p2))
	(setq immtime (getvar "tdusrtimer"))
	(if oldimmtime (setq diftime (* (- immtime oldimmtime) 86400)))
	(defun re1()((lambda (i / e)(while (setq e (ssname ss (setq i (1+ i))))(entupd e)))-1))
	(defun re2()(vl-cmdf "MOVE" ss "" "0,0,0" "0,0,0"))
	(if ss (progn(cond
								 ((null oldimmtime)(setq oldimmtime (getvar "tdusrtimer"))(re1))
								 ((> diftime 0.2)(setq oldimmtime immtime)(re1))
								 (t (setq oldimmtime immtime)(re2))
							 )
					 ;(princ ">>>�ֲ�������")
				 )
		(princ "��Ļ�����������ݣ�")
	)
	(setvar "cmdecho" 1)
	(princ)
)
;�����Զ���������
(defun ChangeFonts (/ file path)
	(setvar "cmdecho" 0)
	(if(null(and
						*fstl_dir*
						(setq path (strcat *fstl_dir* "\\Program\\ChangeFonts\\"))
						(setq file (findfile(strcat path "ChangeFonts2012.dll")))
						(vl-cmdf "netload" file)
						;(if (vl-cmdf "netload" file) t (progn(command-s "netload" file)t))
					))
		(progn
			(av:0totc "tssdeng" "tssdchn" "����")
			(repeat *n-regen* (vla-regen *doc* 0))
		)
	)
	(setvar "cmdecho" 1)
	(vl-acad-undefun 'ChangeFonts)
	(princ)
)
(ChangeFonts)


(princ)



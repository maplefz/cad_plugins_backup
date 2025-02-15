; 定义全局变量存储前缀
(setq *text-prefix* nil)

; 添加前缀功能
(defun c:tjqz ( / prefix ss i ent entdata txt)
  ; 获取前缀
  (setq prefix (getstring "\n请输入要添加的前缀: "))
  
  ; 检查是否有当前选择集，如果没有则提示用户选择
  (setq ss (ssget "_I"))
  (if (null ss)
    (setq ss (ssget))
  )
  
  ; 处理选中的对象
  (setq i 0)
  (repeat (sslength ss)
    (setq ent (ssname ss i))
    (setq entdata (entget ent))
    (setq txt (cdr (assoc 1 entdata)))
    ; 直接修改实体数据
    (entmod (subst (cons 1 (strcat prefix txt))
                   (assoc 1 entdata)
                   entdata))
    (setq i (1+ i))
  )
  (princ)
)

; 添加后缀功能
(defun c:tjhz ( / suffix ss i ent entdata txt)
  ; 获取后缀
  (setq suffix (getstring "\n请输入要添加的后缀: "))
  
  ; 检查是否有当前选择集，如果没有则提示用户选择
  (setq ss (ssget "_I"))
  (if (null ss)
    (setq ss (ssget))
  )
  
  ; 处理选中的对象
  (setq i 0)
  (repeat (sslength ss)
    (setq ent (ssname ss i))
    (setq entdata (entget ent))
    (setq txt (cdr (assoc 1 entdata)))
    ; 直接修改实体数据
    (entmod (subst (cons 1 (strcat txt suffix))
                   (assoc 1 entdata)
                   entdata))
    (setq i (1+ i))
  )
  (princ)
) 

; 显示加载消息
(princ "\n添加前缀命令：tjqz，添加后缀命令：tjhz")

(ns sensitive-filter.dfa
  "敏感词过滤,使用DFA(Deterministic Finite Automaton)算法实现")

(defn word-into-tree
  "将一个敏感词加入到搜索树"
  [tree word]
  (assoc-in tree (conj (vec word) :end?) true))

(comment
  (vec "你好啊")
  => [\你 \好 \啊]

  (word-into-tree {} "你好啊")
  => {\你 {\好 {\啊 {:end? true}}}}

  (-> {}
      (word-into-tree "你好啊")
      (word-into-tree "你好"))
  => {\你 {\好 {:end? true
                \啊 {:end? true}}}})

(defn words->tree
  "敏感词组构造匹配树"
  [words]
  (reduce
    word-into-tree
    {} words))

(comment
  (words->tree ["你好啊" "你好"])
  => {\你 {\好 {:end? true
              \啊 {:end? true}}}})

(defn match-lenth
  "匹配的敏感字长度,返回匹配到的最长的敏感词的长度"
  [tree char-seq matched-lenth]
  (if-let [sub-tree (get tree (first char-seq))]
    (max (if (get sub-tree :end?) (inc matched-lenth) 0)
         (match-lenth sub-tree (rest char-seq) (inc matched-lenth)))
    0))

(comment
  (match-lenth (words->tree ["你好啊" "你好"])
               "你好啊,我很好"
               0)
  => 3 )

(defn sub-replace
  "过滤替换敏感词,使用迭代处理.
  replaced为被替换的map数据结构:
   {:replaced-vec 将敏感词替换为*后的字序列
    :words 包含的敏感词字序列}"
  [tree char-seq replaced]
  (if (empty? char-seq)
    replaced
    (let [len (match-lenth tree char-seq 0)]
      (if (zero? len)
        (sub-replace tree (rest char-seq)
                     (update replaced :replaced-seq concat [(first char-seq)]))
        (sub-replace tree (drop len char-seq)
                     (-> (update replaced :replaced-seq concat (repeat len \*))
                         (update :words conj (take len char-seq))))))))

; 以上的推演:
; 假设char-seq 为 [\我 \想 \你 \好 \的] ,搜索树为 ["你好啊" "你好"] 构造的树
; 第一次 , 从 \我 字搜索树,匹配的长度 len 是 0 ,那么将 \我 字追加到已经替换的内容 replaced-seq
; 然后对 \我 字后面的字序列 [\想 \你 \好 \的] 进行迭代
; 第二次 , \想 字匹配的长度也是 0 ,同样操作
; 第三次 , \你 字开头匹配的长度是 2 ,那么将 两个 \* 追加到已经替换的内容 replaced-seq
; 将字序列 [\你 \好] 加到包含的敏感词字序列 words
; 然后对 \好 字后面的字序列 [\的] 进行迭代
; 最后,返回的结果为 {:replaced-seq (\我 \想 \* \* \的), :words ((\你 \好))}

(comment
  (sub-replace (words->tree ["你好啊" "你好"])
               (vec "我想你好的,你好啊,我很好")
               {:words []})
  => {:words [(\你 \好) (\你 \好 \啊)],
      :replaced-seq (\我 \想 \* \* \的 \, \* \* \* \, \我 \很 \好)})

(defn replace-sensitive
  "过滤字符串中的敏感词"
  [tree string]
  (let [seq->str (fn [seq] (apply str seq))]
    (-> (sub-replace tree (vec string) {:words []})
        (update :replaced-seq seq->str)
        (update :words #(map seq->str %)))))

(comment
  (replace-sensitive (words->tree ["你好啊" "你好"])
                     "我想你好的,你好啊,我很好")
  => {:words ("你好" "你好啊"),
      :replaced-seq "我想**的,***,我很好"})

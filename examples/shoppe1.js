var ApiOutput = {"nodes":[{"data":{"id":"n0","name":"Root","noun":"lightgrey"}},{"data":{"id":"n1","name":"([Category],[Vendor])","noun":"lightgrey"}},{"data":{"id":"n2","name":"[Category]","noun":"lightgrey"}},{"data":{"id":"n3","name":"Category","noun":"lightgrey"}},{"data":{"id":"n4","name":"[Vendor]","noun":"lightgrey"}},{"data":{"id":"n5","name":"Vendor","noun":"lightgrey"}},{"data":{"id":"n6","name":"Product","noun":"lightgrey"}},{"data":{"id":"n7","name":"Cart","noun":"lightgrey"}},{"data":{"id":"n8","name":"Invoice","noun":"green"}},{"data":{"id":"n9","name":"ErrorState","noun":"red"}}],"edges":[{"data":{"source":"n0","target":"n1","label":":<|> (Verb StdMethod * 'GET 200 (': * JSON '[]) ([Category],[Vendor])) (:<|> (:> Symbol * \"categories\" (Verb StdMethod * 'GET 200 (': * JSON '[]) [Category])) (:<|> (:> Symbol * \"categories\" (:> * * (Capture * \"id\" Int) (Verb StdMethod * 'GET 200 (': * JSON '[]) Category))) (:<|> (:> Symbol * \"vendors\" (Verb StdMethod * 'GET 200 (': * JSON '[]) [Vendor])) (:<|> (:> Symbol * \"vendors\" (:> * * (Capture * \"id\" Int) (Verb StdMethod * 'GET 200 (': * JSON '[]) Vendor))) (:<|> (:> Symbol * \"products\" (:> * * (Capture * \"id\" Int) (Verb StdMethod * 'GET 200 (': * JSON '[]) Product))) (:<|> (:> Symbol * \"products\" (:> * * (Capture * \"id\" Int) (:> Symbol * \"add\" (Verb StdMethod * 'POST 200 (': * JSON '[]) Cart)))) (:<|> (:> Symbol * \"cart\" (:> Symbol * \"buy\" (Verb StdMethod * 'POST 200 (': * JSON '[]) Invoice))) (Verb StdMethod * 'GET 200 (': * JSON '[]) ErrorState))))))))","color":"green"}},{"data":{"source":"n1","target":"n2","label":"categories","color":"green"}},{"data":{"source":"n1","target":"n4","label":"vendors","color":"green"}},{"data":{"source":"n2","target":"n3","label":"categories/:id","color":"green"}},{"data":{"source":"n3","target":"n6","label":"products/:id","color":"green"}},{"data":{"source":"n4","target":"n5","label":"vendors/:id","color":"green"}},{"data":{"source":"n5","target":"n6","label":"products/:id","color":"green"}},{"data":{"source":"n6","target":"n7","label":"products/:id/add","color":"purple"}},{"data":{"source":"n7","target":"n8","label":"cart/buy","color":"purple"}},{"data":{"source":"n9","target":"n1","label":":<|> (Verb StdMethod * 'GET 200 (': * JSON '[]) ([Category],[Vendor])) (:<|> (:> Symbol * \"categories\" (Verb StdMethod * 'GET 200 (': * JSON '[]) [Category])) (:<|> (:> Symbol * \"categories\" (:> * * (Capture * \"id\" Int) (Verb StdMethod * 'GET 200 (': * JSON '[]) Category))) (:<|> (:> Symbol * \"vendors\" (Verb StdMethod * 'GET 200 (': * JSON '[]) [Vendor])) (:<|> (:> Symbol * \"vendors\" (:> * * (Capture * \"id\" Int) (Verb StdMethod * 'GET 200 (': * JSON '[]) Vendor))) (:<|> (:> Symbol * \"products\" (:> * * (Capture * \"id\" Int) (Verb StdMethod * 'GET 200 (': * JSON '[]) Product))) (:<|> (:> Symbol * \"products\" (:> * * (Capture * \"id\" Int) (:> Symbol * \"add\" (Verb StdMethod * 'POST 200 (': * JSON '[]) Cart)))) (:<|> (:> Symbol * \"cart\" (:> Symbol * \"buy\" (Verb StdMethod * 'POST 200 (': * JSON '[]) Invoice))) (Verb StdMethod * 'GET 200 (': * JSON '[]) ErrorState))))))))","color":"green"}}]};
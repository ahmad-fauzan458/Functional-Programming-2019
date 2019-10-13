-- 1. Uraikan langkah evaluasi dari ekspresi berikut: [ x+y | x <- [1 .. 4], y <- [2 .. 4], x > y ]
--
-- Pertama tama, dari statement x <- [1..4], y <- [2..4]
-- Kita mendapatkan kombinasi dari dua list tersebut
-- yaitu ketika x = 1 dan y = 2, x = 1 dan y = 3, ... , x = 2 dan y = 2, dan seterusnya
-- Kemudian dari kombinasi tersebut, kita ambil yang sesuai dengan kondisi x > y
-- Terakhir, kita buat list x+y dari kombinasi-kombinasi yang memenuhi kondisi x > y
-- Sehingga hasil akhirnya adalah
nomorSatu = [5, 6, 7]

-- 2. Buatlah fungsi divisor yang menerima sebuah bilangan bulat n dan mengembalikan
-- list bilangan bulat positif yang membagi habis n, Contoh:
-- LatihanLazy> divisor 12
-- [1,2,3,4,6,12]
divisor :: (Integral n) => n -> [n]
divisor n = [x | x <-[1,2..n],  n `mod` x == 0]

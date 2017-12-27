## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = ""
)

## ----chunk and split-----------------------------------------------------
library(keysandstrings)
chunk_string('aaabbbcccddd',  chunk_len=3)
split_key(c('aaabbb', 'aaaccc','bbbaaa'),  chunk_len = 3)

## ----level coder---------------------------------------------------------
data = data.frame(x = rep(c('xkey1', 'xkey2'),2), y = rep(c('ykey1', 'ykey2', 'ykey3', 'ykey4')))
lc = level_coder(data)
x = predict(lc, data)
print(x)
predict(lc, x, rev = TRUE)
key = predict(lc,data, return_primkey = TRUE)
print(key)
predict(lc,key[1], rev = TRUE)

## ----tsfeatures----------------------------------------------------------
head(ts_features(AirPassengers),2)

tail(ts_features(AirPassengers),2)




params =  list(time_col = 'date', 
                    key_col = 'key', 
                    val_col = 'val', 
                    n_future = 2, 
                    n_past = 2,
                    na_fill = NA)
data = data.frame(date = rep(1:10,2), key = c(rep('a',10), rep('b',10) ), val =c(1:10, 2:11))
ts_features(data, params)

# this demonstrates the back fill capability for missing 
data = data.frame(date = c(2:10, 1:10), key = c(rep('a',9), rep('b',10) ), val =c(2:10, 1:10))
new_data = ts_features(data, params)
new_data[ order(new_data$key, new_data$date),]

## ----keyscaler-----------------------------------------------------------
data = as.matrix(data.frame(key = c(rep(1,40), rep(2,40)), 
                  val1 =  c(1:79, NA), 
                  val2 = c(1:80), 
                  val3 = rep(1, 80))) 
head(data)
ks = keyscale(data, key_col = 'key')
x = predict(ks, data)
head(x)
head(predict(ks, x))

## ----black_list----------------------------------------------------------

data = data.frame(a = c(rep('aa', 9), 'b'), b = c(rep('aa', 10)))
bl = black_list(data, cols = c('a', 'b'), freq = 9 )
predict(bl, data, keep_rows = TRUE)
wl = black_list(data, cols = c('a', 'b'), freq = 9, white_list = TRUE )
predict(bl, data, keep_rows = TRUE)


## ----sparse encoder------------------------------------------------------
data = data.frame(x = rep(x = c(NA,letters[1:4]), 5), y = rep(c(NA,letters[5:8]), 5), z = c(1:24, NA), zz = 1:25)
lc = sparseEncoder(data)
predict(lc, data)


## ----find strings--------------------------------------------------------
data = c('aabb', 'aacc', 'aaaa', 'bbaa','bbbb','bbcc')
preFixes = c('aa', 'bb')


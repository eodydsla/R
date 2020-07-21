# col1, col2에 따라 값 합 구하기
# 숫자가  자동으로 생략됨
aggregate(.~col1+col2, Cars93, sum)

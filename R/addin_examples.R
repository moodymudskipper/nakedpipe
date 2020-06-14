#
# library(nakedpipe)
#
# # Assign a keyboard shortcut to addin
# # => toggle seamlessly between magrittr syntax and nakedpipe syntax
#
# # standard chain
# cars %>%
#   head() %>%
#   dim()
#
# #assignment pipes
# cars %<>%
#   head() %>%
#   dim()
#
# # selection starting with assignment
# test <- cars %>%
#   head() %>%
#   dim()
#
# # "with"/"%$%"
# cars %>%
#   head() %$%
#   mean(dist)
#
# # pipe to ggplot calls
# cars %>%
#   head() %>%
#   ggplot(aes(speed, dist)) +
#   geom_point()
#
# # side effects
# cars %>%
#   head(2) %T>%
#   print(.) %>%
#   dim()
#
#
#
#
#
#
#
#
#
#
#
#
#
#

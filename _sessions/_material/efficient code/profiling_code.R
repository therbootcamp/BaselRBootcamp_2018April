########################## Profiling code ################################

my_expression <- expression({
  
  # load data
  data <- read_csv('https://therbootcamp.github.io/_slides/data/titanic.csv')
  
  # remove first column
  data <- data[,-1]
  
  # mutate
  data <- data %>% 
    mutate(months = Age * 12)
  
  # select
  test_data <- data %>% 
    select(Sex,Age,Survived)
  
  # multiple regression
  model <- lm(Survived ~ Sex * Age, 
              data = test_data, 
              family = 'binomial')
  
  # evaluate model
  summary(model)
})

lineprof(eval(my_expression))

#' KNN Predict (No SHAP)
#'
#' 给定 8 个特征的数值输入，输出该样本预测概率与分类结果。
#'
#' @param new_data data.frame，包含列: ABCB5, AC093642.5, AF146191.4, ANO3, 
#'        CTD.2231H16.1, PRKG1, RASA3, VIPR2
#' @return List: 包含 pred_prob, pred_class
#'
#' @import kknn
#' @export
predict_result <- function(new_data) {
  
  # 1. 检查输入数据格式
  required_cols <- c("ABCB5", "AC093642.5", "AF146191.4", "ANO3", 
                     "CTD.2231H16.1", "PRKG1", "RASA3", "VIPR2")
  if (!all(required_cols %in% colnames(new_data))) {
    stop("new_data 必须包含以下列: ", paste(required_cols, collapse = ", "))
  }
  
  # 2. 加载训练集 & k 值 & 均值、标准差
  data("traindata_scaled", package = "MyKNNPackage") 
  data("best_k_auc",       package = "MyKNNPackage") 
  data("means", "sds",     package = "MyKNNPackage")
  
  # 3. 对 new_data 做和训练集相同的标准化
  for (col_i in required_cols) {
    new_data[[col_i]] <- (new_data[[col_i]] - means[col_i]) / sds[col_i]
  }
  
  # 4. 利用 kknn 做预测 (train=traindata_scaled, test=new_data)
  #    假设 traindata_scaled 第 1 列是 Result，后面是那 8 个特征
  model_test <- kknn(
    formula = Result ~ .,
    train   = traindata_scaled, 
    test    = new_data,
    k       = best_k_auc, 
    kernel  = "triangular"
  )
  
  # 预测属于Yes的概率
  pred_prob_matrix <- predict(model_test, type = "prob")
  pred_probs <- pred_prob_matrix[, "Yes"]
  
  # 二分类阈值
  pred_class <- ifelse(pred_probs >= 0.5, "Yes", "No")
  
  # 5. 返回预测结果
  return(list(
    pred_prob  = pred_probs,
    pred_class = pred_class
  ))
}
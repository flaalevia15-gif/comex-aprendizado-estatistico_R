# ==============================================================================
# SCRIPT MODULAR: REGRESSÃO LOGÍSTICA BINÁRIA
# ==============================================================================
#
# ÍNDICE DE SEÇÕES:
# 1. [TAG: CONFIGURACAO]
# 2. [TAG: PREPARACAO_DADOS]
# 3. [TAG: REGRESSAO_LOGISTICA_SIMPLES]
# 4. [TAG: REGRESSAO_LOGISTICA_MULTIPLA]
# 5. [TAG: METRICAS_E_AVALIACAO]
# 6. [TAG: CURVA_ROC_E_AUC]
# 7. [TAG: VISUALIZACOES]
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. [TAG: CONFIGURACAO]
# ------------------------------------------------------------------------------

configurar_ambiente <- function() {
  options(scipen = 999, digits = 4)
  set.seed(42)
  message("[OK] Ambiente configurado com sucesso para Regressão Logística.")
}

# ------------------------------------------------------------------------------
# 2. [TAG: PREPARACAO_DADOS]
# ------------------------------------------------------------------------------

#' Cria a variável resposta binária de crescimento anual FOB (t vs t-1)
#' @param df Data frame contendo as colunas originais
#' @return Data frame filtrado contendo a variável Crescimento_binario
preparar_dados_crescimento <- function(df) {
  # Extração segura das colunas com os nomes exatos da base
  df_model <- data.frame(
    Municipio = as.factor(df[["Município"]]),
    Pais = as.factor(df[["País"]]),
    SH4 = as.character(df[["Código SH4"]]),
    Ano = as.integer(df[["Ano"]]),
    Valor_FOB = as.numeric(df[["Valor US$ FOB"]]),
    Quilograma_Liquido = as.numeric(df[["Quilograma Líquido"]]),
    stringsAsFactors = FALSE
  )
  
  # Ordenação temporal por série/produto (SH4 + Município + País + Ano)
  df_model <- df_model[order(df_model$SH4, df_model$Municipio, df_model$Pais, df_model$Ano), ]
  
  # Criação da chave identificadora da série temporal
  df_model$id_serie <- paste(df_model$SH4, df_model$Municipio, df_model$Pais, sep = "_")
  
  # Identificação do registro anterior (lag)
  n <- nrow(df_model)
  df_model$id_serie_ant <- c(NA, df_model$id_serie[-n])
  df_model$Ano_ant <- c(NA, df_model$Ano[-n])
  df_model$Valor_FOB_ant <- c(NA, df_model$Valor_FOB[-n])
  
  # Validação: mesmo grupo e ano consecutivo (t vs t-1)
  df_model$valido <- (!is.na(df_model$id_serie_ant)) & 
                     (df_model$id_serie == df_model$id_serie_ant) & 
                     (df_model$Ano == df_model$Ano_ant + 1)
  
  # Filtra apenas observações que possuem ano base anterior válido
  df_model <- df_model[df_model$valido, ]
  
  # Crescimento: 1 se FOB_t > FOB_{t-1}, 0 se menor ou igual
  df_model$Crescimento_binario <- ifelse(df_model$Valor_FOB > df_model$Valor_FOB_ant, 1L, 0L)
  
  # Classificação SH4 em Commodity vs Manufaturado (Capítulos SH2 <= 27 como Commodities)
  sh2 <- as.integer(substr(sprintf("%04d", as.integer(df_model$SH4)), 1, 2))
  df_model$Categoria_SH4 <- factor(ifelse(sh2 <= 27, "Commodity", "Manufaturado"))
  
  # Limpeza das colunas auxiliares
  df_model$id_serie <- NULL
  df_model$id_serie_ant <- NULL
  df_model$Ano_ant <- NULL
  df_model$Valor_FOB_ant <- NULL
  df_model$valido <- NULL
  
  return(df_model)
}

# ------------------------------------------------------------------------------
# 3. [TAG: REGRESSAO_LOGISTICA_SIMPLES]
# ------------------------------------------------------------------------------

#' Ajusta o modelo logístico simples: Fluxo ~ Quilograma Líquido
#' @param df Data frame contendo Fluxo e Quilograma Líquido
#' @return Objeto glm ajustado
ajustar_logistica_simples <- function(df) {
  y_raw <- df[["Fluxo"]]
  y_bin <- if (is.factor(y_raw) || is.character(y_raw)) {
    ifelse(grepl("Exp", as.character(y_raw), ignore.case = TRUE), 1L, 0L)
  } else {
    as.integer(y_raw)
  }
  
  df_ajuste <- data.frame(
    Fluxo_bin = y_bin,
    Quilograma_Liquido = as.numeric(df[["Quilograma Líquido"]])
  )
  
  modelo <- glm(Fluxo_bin ~ Quilograma_Liquido, data = df_ajuste, family = binomial(link = "logit"))
  return(modelo)
}

# ------------------------------------------------------------------------------
# 4. [TAG: REGRESSAO_LOGISTICA_MULTIPLA]
# ------------------------------------------------------------------------------

#' Ajusta o modelo logístico múltiplo: Crescimento_binario ~ Municipio + Pais + Categoria_SH4 + Quilograma_Liquido
#' @param df Data frame retornado por preparar_dados_crescimento()
#' @return Objeto glm ajustado
ajustar_logistica_multipla <- function(df) {
  modelo <- glm(Crescimento_binario ~ Municipio + Pais + Categoria_SH4 + Quilograma_Liquido,
                data = df, 
                family = binomial(link = "logit"))
  return(modelo)
}

# ------------------------------------------------------------------------------
# 5. [TAG: METRICAS_E_AVALIACAO]
# ------------------------------------------------------------------------------

#' Extrai coeficientes, Odds Ratios (e^beta) e significância estatística tratando singularidades
#' @param modelo Objeto glm
#' @return Data frame com coeficientes formatados
sumarizar_odds_ratios <- function(modelo) {
  resumo <- as.data.frame(summary(modelo)$coefficients)
  termos_validos <- rownames(resumo)
  
  # Calcula intervalos de confiança apenas para os termos estimados
  ci <- suppressMessages(confint.default(modelo))
  ci_validos <- ci[termos_validos, , drop = FALSE]
  
  tabela <- data.frame(
    Termo = termos_validos,
    Estimativa_Logit = round(resumo[, 1], 4),
    Erro_Padrao = round(resumo[, 2], 4),
    Valor_z = round(resumo[, 3], 3),
    p_valor = format.pval(resumo[, 4], digits = 3, eps = 0.001),
    Odds_Ratio = round(exp(resumo[, 1]), 4),
    CI_2.5 = round(exp(ci_validos[, 1]), 4),
    CI_97.5 = round(exp(ci_validos[, 2]), 4),
    stringsAsFactors = FALSE
  )
  
  rownames(tabela) <- NULL
  return(tabela)
}
#' Calcula matriz de confusão e métricas para múltiplos limiares de decisão
#' @param modelo Objeto glm
#' @param y_real Vetor de valores reais (0 e 1)
#' @param limiares Vetor de limiares numéricos (ex: c(0.3, 0.5, 0.7))
#' @return Lista contendo tabelas e métricas comparativas
avaliar_desempenho_classificacao <- function(modelo, y_real, limiares = c(0.3, 0.5, 0.7)) {
  prob_est <- predict(modelo, type = "response")
  metricas_lista <- list()
  
  for (c_limiar in limiares) {
    y_pred <- as.integer(prob_est > c_limiar)
    matriz <- table(Real = y_real, Previsto = factor(y_pred, levels = c(0, 1)))
    
    vn <- matriz[1, 1]
    fp <- matriz[1, 2]
    fn <- matriz[2, 1]
    vp <- matriz[2, 2]
    total <- sum(matriz)
    
    acuracia <- (vp + vn) / total
    precisao <- ifelse((vp + fp) > 0, vp / (vp + fp), 0)
    recall_sensibilidade <- ifelse((vp + fn) > 0, vp / (vp + fn), 0)
    especificidade <- ifelse((vn + fp) > 0, vn / (vn + fp), 0)
    
    metricas_lista[[as.character(c_limiar)]] <- list(
      Limiar = c_limiar,
      Matriz = matriz,
      Acuracia = round(acuracia, 4),
      Precisao = round(precisao, 4),
      Recall = round(recall_sensibilidade, 4),
      Especificidade = round(especificidade, 4)
    )
  }
  return(metricas_lista)
}

# ------------------------------------------------------------------------------
# 6. [TAG: CURVA_ROC_E_AUC]
# ------------------------------------------------------------------------------

#' Calcula e plota a Curva ROC e o valor da AUC
#' @param modelo Objeto glm
#' @param y_real Vetor de valores reais (0 e 1)
#' @param titulo Título do gráfico
#' @return Valor numérico da AUC
plotar_curva_roc <- function(modelo, y_real, titulo = "Curva ROC") {
  prob_est <- predict(modelo, type = "response")
  
  # Ordenação decrescente de probabilidade
  ord <- order(prob_est, decreasing = TRUE)
  y_ord <- y_real[ord]
  
  tpr <- cumsum(y_ord) / sum(y_ord)
  fpr <- cumsum(1 - y_ord) / sum(1 - y_ord)
  
  # Cálculo exato de AUC por concordância empírica
  auc_val <- mean(outer(prob_est[y_real == 1], prob_est[y_real == 0], ">"))
  
  # Plot da Curva
  par(mar = c(4.5, 4.5, 2, 2), pty = "s")
  plot(c(0, fpr), c(0, tpr), type = "l", lwd = 3, col = "#E66101",
       xlab = "Taxa de Falsos Positivos (1 - Especificidade)",
       ylab = "Taxa de Verdadeiros Positivos (Sensibilidade / Recall)",
       main = paste0(titulo, " (AUC = ", round(auc_val, 4), ")"))
  abline(0, 1, lty = 2, col = "gray50")
  
  # Destacar limiares de corte: 0.7, 0.5, 0.3
  for (L in c(0.7, 0.5, 0.3)) {
    pt_fpr <- mean(prob_est[y_real == 0] > L)
    pt_tpr <- mean(prob_est[y_real == 1] > L)
    points(pt_fpr, pt_tpr, pch = 19, cex = 1.3, col = "#5E3C99")
    text(pt_fpr, pt_tpr, labels = paste0("c=", L), pos = 4, cex = 0.8, col = "#5E3C99")
  }
  grid()
  
  return(auc_val)
}

# ------------------------------------------------------------------------------
# 7. [TAG: VISUALIZACOES]
# ------------------------------------------------------------------------------

#' Plota a curva logística sigmoide para o modelo simples
#' @param modelo Objeto glm simples
#' @param df Data frame utilizado
plotar_sigmoide_simples <- function(modelo, df) {
  b0 <- coef(modelo)[1]
  b1 <- coef(modelo)[2]
  x_star <- -b0 / b1
  
  x_vals <- as.numeric(df[["Quilograma Líquido"]])
  x_range <- seq(min(x_vals, na.rm = TRUE), max(x_vals, na.rm = TRUE), length.out = 300)
  
  p_pred <- predict(modelo, newdata = data.frame(Quilograma_Liquido = x_range), type = "response")
  
  y_raw <- df[["Fluxo"]]
  y_real <- if (is.factor(y_raw) || is.character(y_raw)) {
    ifelse(grepl("Exp", as.character(y_raw), ignore.case = TRUE), 1, 0)
  } else {
    as.integer(y_raw)
  }
  
  par(mar = c(4.5, 4.5, 2, 2), pty = "m")
  plot(x_vals, y_real, pch = 21, bg = rgb(0.2, 0.4, 0.8, 0.4), col = "transparent",
       xlab = "Quilograma Líquido (kg)", ylab = "P(Fluxo = Exportação)",
       main = "Ajuste Logístico: Probabilidade de Exportação por Peso")
  lines(x_range, p_pred, col = "#D95F02", lwd = 3)
  abline(h = c(0, 1), lty = 3, col = "gray60")
  abline(h = 0.5, lty = 2, col = "gray40")
  
  if (!is.na(x_star) && x_star >= min(x_vals) && x_star <= max(x_vals)) {
    abline(v = x_star, lty = 2, col = "#7570B3")
    points(x_star, 0.5, pch = 19, col = "#7570B3", cex = 1.4)
    legend("bottomright", legend = c("Ajuste Sigmoide", "Fronteira (p=0.5)"),
           col = c("#D95F02", "#7570B3"), lty = c(1, 2), lwd = c(3, 1), bty = "n")
  }
}
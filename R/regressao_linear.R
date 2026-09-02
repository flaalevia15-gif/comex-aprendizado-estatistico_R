# ==============================================================================
# SCRIPT MODULAR: MODELAGEM ECONOMÉTRICA E CLASSIFICAÇÃO (R/regressoes.R)
# ==============================================================================
# 
# ÍNDICE DE SEÇÕES:
# 1. [TAG: REGRESSAO_SIMPLES]   - Regressão Linear Simples por Rota (Valor FOB ~ Volume)
# 2. [TAG: DIAGNOSTICO_SIMPLES] - Diagnósticos Residuais do OLS Simples
# 3. [TAG: REGRESSAO_MULTIPLA]  - Regressão Linear Múltipla Consolidada (Dummies de Rota/Município/Fluxo)
# 4. [TAG: DIAGNOSTICO_MULTIPLA]- Diagnóstico Residual da Regressão Múltipla
# 5. [TAG: TESTE_HETEROCEDASTICIDADE] TESTE DE BREUSCH-PAGAN (BP-TEST)
# ==============================================================================

library(dplyr)
library(stringr)
library(ggplot2)
library(scales)
library(lmtest)


# ==============================================================================
# 1. [TAG: REGRESSAO_SIMPLES] REGRESSÃO LINEAR SIMPLES POR ROTA LÍDER (TOP 1)
# ==============================================================================

#' Ajusta modelos OLS simples (Valor FOB ~ Volume) para cada uma das 6 rotas líderes
#' @param df_sazonal_lideres Tibble contendo os dados mensais agregados das rotas líderes
#' @return Lista contendo: metricas, modelos (objetos lm) e dados formatados
ajustar_regressao_top1_rotas <- function(df_sazonal_lideres) {
  df_modelagem <- df_sazonal_lideres |>
    mutate(
      volume_mil_ton = `Quilograma Líquido` / 1e6,
      valor_milhoes_fob = `Valor US$ FOB` / 1e6
    )
  
  resultados <- df_modelagem |>
    group_by(Fluxo, Município, País, `Código SH4`, `Descrição SH4`) |>
    group_split()
  
  lista_metricas <- list()
  lista_lm <- list()
  
  for (i in seq_along(resultados)) {
    dados_rota <- resultados[[i]]
    rota_id <- paste(dados_rota$Município[1], dados_rota$Fluxo[1], dados_rota$País[1], sep = " | ")
    
    # Y = Valor FOB (Milhões US$), X = Volume (Milhares de Toneladas)
    modelo <- lm(valor_milhoes_fob ~ volume_mil_ton, data = dados_rota)
    resumo <- summary(modelo)
    
    lista_metricas[[i]] <- tibble(
      Município = dados_rota$Município[1],
      Fluxo = dados_rota$Fluxo[1],
      País = dados_rota$País[1],
      `Código SH4` = dados_rota$`Código SH4`[1],
      `Descrição SH4` = dados_rota$`Descrição SH4`[1],
      N = nrow(dados_rota),
      Intercepto_b0 = round(coef(modelo)[1], 4),
      Inclinacao_b1_preco_medio = round(coef(modelo)[2], 4),
      `R2` = round(resumo$r.squared, 4),
      `R2_Ajustado` = round(resumo$adj.r.squared, 4),
      `RMSE_Milhoes_FOB` = round(sqrt(mean(resumo$residuals^2)), 4),
      `p_valor` = format.pval(resumo$coefficients[2, 4], eps = 0.001, digits = 3)
    )
    lista_lm[[rota_id]] <- modelo
  }
  
  list(
    metricas = bind_rows(lista_metricas),
    modelos = lista_lm,
    dados = df_modelagem
  )
}

#' Plota os diagramas de dispersão com as retas de regressão (Valor FOB ~ Volume)
#' @param resultado_regressao Objeto retornado por ajustar_regressao_top1_rotas()
#' @return Objeto ggplot facetado
plotar_regressao_top1_rotas <- function(resultado_regressao) {
  df_dados <- resultado_regressao$dados
  df_rotulos <- resultado_regressao$metricas |>
    mutate(
      rotulo = sprintf(
        "%s | %s\n%s (SH4 %s)\nR² = %.3f | b1 = %.2f M$/k ton | RMSE = %.2f M$",
        Município, Fluxo, País, `Código SH4`,
        R2, Inclinacao_b1_preco_medio, RMSE_Milhoes_FOB
      )
    )
  
  df_plot <- df_dados |>
    left_join(
      df_rotulos |> select(Município, Fluxo, País, `Código SH4`, rotulo),
      by = c("Município", "Fluxo", "País", "Código SH4")
    )
  
  ggplot(df_plot, aes(x = volume_mil_ton, y = valor_milhoes_fob)) +
    geom_smooth(method = "lm", formula = y ~ x, color = "#2980b9", fill = "#3498db", alpha = 0.2, linewidth = 0.9) +
    geom_point(color = "#2c3e50", alpha = 0.7, size = 1.8) +
    facet_wrap(~ rotulo, scales = "free", ncol = 2) +
    scale_x_continuous(labels = label_number(suffix = " k ton", big.mark = ".")) +
    scale_y_continuous(labels = label_number(suffix = " M$", big.mark = ".")) +
    labs(
      title = "Regressão Linear Simples por Rota Líder",
      subtitle = "Modelo OLS: Valor Comercial (Milhões US$ FOB) ~ Volume Físico (k ton)",
      x = "Volume Físico (Milhares de Toneladas)",
      y = "Valor Comercial (Milhões de US$ FOB)"
    ) +
    theme_minimal() +
    theme(
      strip.background = element_rect(fill = "#f8f9fa", color = "#dee2e6"),
      strip.text = element_text(face = "bold", size = 8.5, lineheight = 1.1),
      panel.grid.minor = element_blank()
    )
}


# ==============================================================================
# 2. [TAG: DIAGNOSTICO_SIMPLES] DIAGNÓSTICOS DO MODELO OLS SIMPLES
# ==============================================================================

#' Extrai resíduos e valores ajustados para os modelos simples
obter_dados_diagnostico_rotas <- function(resultado_regressao) {
  df_dados <- resultado_regressao$dados
  
  df_dados |>
    group_by(Fluxo, Município, País, `Código SH4`, `Descrição SH4`) |>
    group_modify(~ {
      mod <- lm(valor_milhoes_fob ~ volume_mil_ton, data = .x)
      .x |>
        mutate(
          Data = as.Date(paste(Ano, mes_numero, "01", sep = "-")),
          Valor_Real = valor_milhoes_fob,
          Ajuste_OLS = fitted(mod),
          Residuos = resid(mod)
        )
    }) |>
    ungroup() |>
    mutate(
      rotulo_faceta = sprintf("%s | %s\n%s — SH4 %s", Município, Fluxo, País, `Código SH4`)
    )
}

#' Gráfico Resíduos vs. Valores Ajustados (OLS Simples)
plotar_todos_residuos_vs_ajustados <- function(df_diagnostico) {
  ggplot(df_diagnostico, aes(x = Ajuste_OLS, y = Residuos)) +
    geom_hline(yintercept = 0, color = "#d62728", linetype = "dashed", linewidth = 0.8) +
    geom_point(color = "#1f77b4", alpha = 0.8, size = 2) +
    facet_wrap(~ rotulo_faceta, scales = "free", ncol = 2) +
    labs(
      title = "Resíduos vs. Valores Ajustados por Rota Líder (OLS Simples)",
      subtitle = "Homocedasticidade e linearidade avaliadas em Milhões de US$ FOB",
      x = expression("Valores Ajustados (" * hat(Y)[t] * ") [Milhões US$]"),
      y = expression("Resíduos (" * e[t] * ") [Milhões US$]")
    ) +
    theme_minimal(base_size = 11) +
    theme(
      strip.background = element_rect(fill = "#f8f9fa", color = "#dee2e6"),
      strip.text = element_text(face = "bold", size = 8.5, lineheight = 1.1),
      panel.grid.minor = element_blank()
    )
}

# ==============================================================================
# 3. [TAG: REGRESSAO_MULTIPLA] REGRESSÃO LINEAR CONSOLIDADA COM INTERAÇÃO
# ==============================================================================

#' Ajusta o modelo consolidado integrando as 6 rotas líderes com termos de interação
#' @param df_sazonal_lideres Tibble contendo os dados mensais agregados
#' @return Lista contendo: modelo lm, tabela_coeficientes, metricas_globais e base modelada
ajustar_regressao_multipla_consolidada <- function(df_sazonal_lideres) {
  df_multipla <- df_sazonal_lideres |>
    mutate(
      volume_mil_ton = `Quilograma Líquido` / 1e6,
      valor_milhoes_fob = `Valor US$ FOB` / 1e6,
      # Cria um fator identificador único para cada uma das 6 combinações
      rota_lider = factor(paste(Município, Fluxo, País, str_trunc(`Descrição SH4`, 15), sep = " | "))
    )
  
  # Modelo com interação total: permite intercepto e inclinação (preço/ton) próprios por rota
  modelo_multiplo <- lm(
    valor_milhoes_fob ~ volume_mil_ton * rota_lider,
    data = df_multipla
  )
  
  resumo_m <- summary(modelo_multiplo)
  
  # Tabela detalhada de coeficientes e termos de interação formatada
  tabela_coeficientes <- as.data.frame(resumo_m$coefficients) |>
    tibble::rownames_to_column("Termo") |>
    mutate(
      # Corrige o texto juntado:
      # 1. Substitui "volume_mil_ton:rota_lider" por "[Interação] Volume x "
      Termo = stringr::str_replace(Termo, "volume_mil_ton:rota_lider", "[Interação] Volume × "),
      # 2. Substitui "rota_lider" inicial por "Rota: " (adicionando espaço)
      Termo = stringr::str_replace(Termo, "^rota_lider", "Rota: "),
      # 3. Dá um nome mais amigável para o Volume principal
      Termo = stringr::str_replace(Termo, "^volume_mil_ton$", "Volume (k ton)")
    ) |>
    rename(
      Estimativa = Estimate,
      Erro_Padrao = `Std. Error`,
      t_valor = `t value`,
      p_valor = `Pr(>|t|)`
    ) |>
    mutate(
      Estimativa = round(Estimativa, 4),
      Erro_Padrao = round(Erro_Padrao, 4),
      t_valor = round(t_valor, 3),
      p_valor_formatado = format.pval(p_valor, eps = 0.001, digits = 3)
    )
  
  # Métricas globais de ajuste
  f_stat <- resumo_m$fstatistic
  p_val_global <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
  
  metricas_globais <- tibble(
    N_Observacoes = nrow(df_multipla),
    R2 = round(resumo_m$r.squared, 4),
    R2_Ajustado = round(resumo_m$adj.r.squared, 4),
    RMSE_Milhoes_FOB = round(sqrt(mean(resumo_m$residuals^2)), 4),
    F_Estatistica = round(f_stat[1], 2),
    p_valor_Global = format.pval(p_val_global, eps = 0.001, digits = 3)
  )
  
  list(
    modelo = modelo_multiplo,
    coeficientes = tabela_coeficientes,
    metricas_globais = metricas_globais,
    dados = df_multipla
  )
}


# ==============================================================================
# 4. [TAG: DIAGNOSTICO_MULTIPLA] DIAGNÓSTICO DO MODELO CONSOLIDADO
# ==============================================================================

#' Plota a aderência Real vs. Ajustado por rota e fluxo
plotar_diagnostico_multipla <- function(resultado_multipla) {
  df_plot <- resultado_multipla$dados |>
    mutate(
      Valor_Previsto = fitted(resultado_multipla$modelo),
      Residuos = resid(resultado_multipla$modelo)
    )
  
  ggplot(df_plot, aes(x = Valor_Previsto, y = valor_milhoes_fob, color = Fluxo)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#7f8c8d", linewidth = 0.8) +
    geom_point(alpha = 0.7, size = 2) +
    facet_wrap(~ rota_lider, scales = "free", ncol = 2) +
    scale_x_continuous(labels = label_number(suffix = " M$", big.mark = ".", decimal.mark = ",")) +
    scale_y_continuous(labels = label_number(suffix = " M$", big.mark = ".", decimal.mark = ",")) +
    scale_color_manual(values = c("Exportação" = "#27ae60", "Importação" = "#e67e22")) +
    labs(
      title = "Modelo Múltiplo Consolidado: Valor Real vs. Valor Previsto",
      subtitle = "Ajuste com termos de interação (Volume * Rota Líder)",
      x = "Valor FOB Previsto (Milhões US$)",
      y = "Valor FOB Real (Milhões US$)"
    ) +
    theme_minimal() +
    theme(
      strip.background = element_rect(fill = "#f8f9fa", color = "#dee2e6"),
      strip.text = element_text(face = "bold", size = 8),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}

# ==============================================================================
# 5. [TAG: TESTE_HETEROCEDASTICIDADE] TESTE DE BREUSCH-PAGAN (BP-TEST)
# ==============================================================================

#' Executa o teste de Breusch-Pagan para os modelos simples e consolidado
#' @param resultado_simples Objeto retornado por ajustar_regressao_top1_rotas()
#' @param resultado_multipla Objeto retornado por ajustar_regressao_multipla_consolidada()
#' @return Lista contendo tabela dos modelos simples e resultado do modelo múltiplo
executar_teste_breusch_pagan <- function(resultado_simples, resultado_multipla) {
  
  # 1. Teste para cada uma das 6 rotas líderes (Regressão Simples)
  lista_bp_simples <- list()
  
  for (nome_rota in names(resultado_simples$modelos)) {
    mod <- resultado_simples$modelos[[nome_rota]]
    bp <- lmtest::bptest(mod)
    
    lista_bp_simples[[nome_rota]] <- tibble(
      Rota = nome_rota,
      `BP Estatística (LM)` = round(as.numeric(bp$statistic), 3),
      `Graus de Liberdade` = as.numeric(bp$parameter),
      `p-valor` = format.pval(bp$p.value, eps = 0.001, digits = 3),
      `Diagnóstico (α = 5%)` = ifelse(bp$p.value < 0.05, "Heterocedástico", "Homocedástico")
    )
  }
  
  tabela_bp_simples <- bind_rows(lista_bp_simples)
  
  # 2. Teste para o modelo Múltiplo Consolidado
  bp_mult <- lmtest::bptest(resultado_multipla$modelo)
  tabela_bp_multipla <- tibble(
    Modelo = "Múltiplo Consolidado (Interações)",
    `BP Estatística (LM)` = round(as.numeric(bp_mult$statistic), 3),
    `Graus de Liberdade` = as.numeric(bp_mult$parameter),
    `p-valor` = format.pval(bp_mult$p.value, eps = 0.001, digits = 3),
    `Diagnóstico (α = 5%)` = ifelse(bp_mult$p.value < 0.05, "Heterocedástico", "Homocedástico")
  )
  
  list(
    simples = tabela_bp_simples,
    multiplo = tabela_bp_multipla
  )
}
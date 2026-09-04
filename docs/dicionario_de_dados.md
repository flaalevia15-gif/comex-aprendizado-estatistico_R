# Dicionário de Dados — Comércio Exterior da Baixada Santista (2019–2026)

Este documento padroniza a estrutura dos dados brutos e tratados do projeto de análise de Comércio Exterior da Baixada Santista, seguindo a classificação estatística de variáveis (**Variável**, **Descrição**, **Tipo** e **Unidade**).

---

## 1. Dados de Entrada / Brutos (`data/raw/`)

Origem dos dados: Microdados abertos do sistema **Comex Stat** (Ministério do Desenvolvimento, Indústria, Comércio e Serviços - MDIC).

### 1.1. Base Regional de Recorte (`toda_baixada_2019-01_2026-07.csv`)
Base agregada contendo todos os municípios da Região Metropolitana da Baixada Santista, utilizada para a justificativa estatística do recorte geográfico.

| Variável | Descrição | Tipo | Unidade |
| :--- | :--- | :--- | :--- |
| `Ano` | Ano de registro da operação comercial (2019 a 2026) | quantitativa discreta | ano |
| `Fluxo` | Sentido da operação comercial (`Exportação` ou `Importação`) | qualitativa nominal | — |
| `Município` | Nome do município da Baixada Santista com UF | qualitativa nominal | — |
| `Valor US$ FOB` | Valor financeiro da mercadoria em dólares americanos (FOB) | quantitativa contínua | US$ |

---

### 1.2. Bases Particionadas Detalhadas (`2019-01_2020-12.csv`, `2021-01_2022-12.csv`, `2023-01_2024-12.csv`, `2025-01_2026-07.csv`)
Arquivos particionados por períodos com as transações dos principais polos (Santos, Cubatão e Guarujá).

| Variável | Descrição | Tipo | Unidade |
| :--- | :--- | :--- | :--- |
| `Ano` | Ano de registro da operação comercial (2019 a 2026) | quantitativa discreta | ano |
| `Mês` | Mês da operação no formato ordinal e abreviação (ex.: `01. Jan`) | qualitativa ordinal | mês |
| `Fluxo` | Sentido da operação comercial (`Exportação` ou `Importação`) | qualitativa nominal | — |
| `Município` | Município polo analisado com UF | qualitativa nominal | — |
| `País` | País parceiro de destino (exportação) ou origem (importação) | qualitativa nominal | — |
| `Código SH4` | Código do Sistema Harmonizado no nível de 4 dígitos (Posição) | qualitativa nominal | — |
| `Descrição SH4` | Descrição oficial da mercadoria associada ao código SH4 | qualitativa nominal | — |
| `Valor US$ FOB` | Valor financeiro da mercadoria em dólares americanos (FOB) | quantitativa contínua | US$ |
| `Quilograma Líquido` | Peso líquido total da mercadoria movimentada | quantitativa contínua | kg |

---

## 2. Dados Processados (`data/processed/` e Memória)

### 2.1. Base Consolidada (`dados_completos_2019_2026.csv`)
Junção de todas as partições temporais brutas com padronização dos meses.

| Variável | Descrição | Tipo | Unidade |
| :--- | :--- | :--- | :--- |
| `Fluxo` | Sentido da operação comercial (`Exportação` ou `Importação`) | qualitativa nominal | — |
| `Ano` | Ano de registro da operação comercial | quantitativa discreta | ano |
| `Município` | Município polo analisado | qualitativa nominal | — |
| `Código SH4` | Código do Sistema Harmonizado de 4 dígitos | qualitativa nominal | — |
| `Descrição SH4` | Descrição oficial da mercadoria | qualitativa nominal | — |
| `País` | País parceiro comercial | qualitativa nominal | — |
| `Valor US$ FOB` | Valor financeiro da operação comercial (FOB) | quantitativa contínua | US$ |
| `Quilograma Líquido` | Peso líquido da mercadoria | quantitativa contínua | kg |
| `mes_numero` | Número ordinal do mês (1 a 12) | quantitativa discreta | mês |
| `mes_nome` | Nome/abreviação textual do mês (ex.: `Jan`, `Fev`) | qualitativa ordinal | — |

---

### 2.2. Tabela Fato Transacional (`df_transacional`)
Estrutura transacional normalizada em nível de município, parceiro, produto e período.

| Variável | Descrição | Tipo | Unidade |
| :--- | :--- | :--- | :--- |
| `Fluxo` | Sentido da operação comercial (`Exportação` ou `Importação`) | qualitativa nominal | — |
| `Ano` | Ano de registro da operação comercial | quantitativa discreta | ano |
| `mes_numero` | Número ordinal do mês (1 a 12) | quantitativa discreta | mês |
| `mes_nome` | Nome textual do mês | qualitativa ordinal | — |
| `Município` | Município polo analisado | qualitativa nominal | — |
| `País` | País parceiro comercial | qualitativa nominal | — |
| `Código SH4` | Código numérico da posição SH4 (Chave Estrangeira) | qualitativa nominal | — |
| `Valor US$ FOB` | Valor financeiro da operação (FOB) | quantitativa contínua | US$ |
| `Quilograma Líquido` | Peso líquido da mercadoria | quantitativa contínua | kg |

---

### 2.3. Tabela Dimensão Produto (`df_dimensao_sh4`)
Tabela de referência e catálogo dimensional de mercadorias.

| Variável | Descrição | Tipo | Unidade |
| :--- | :--- | :--- | :--- |
| `Código SH4` | Código identificador único do produto (Chave Primária) | qualitativa nominal | — |
| `Descrição SH4` | Descrição textual oficial da posição SH4 | qualitativa nominal | — |

---

### 2.4. Tabela Fato Agregada Temporal (`df_sazonal`)
Agregação mensal por fluxo e município para análise temporal e sazonal.

| Variável | Descrição | Tipo | Unidade |
| :--- | :--- | :--- | :--- |
| `Fluxo` | Sentido da operação comercial (`Exportação` ou `Importação`) | qualitativa nominal | — |
| `Ano` | Ano de registro da agregação | quantitativa discreta | ano |
| `mes_numero` | Número ordinal do mês (1 a 12) | quantitativa discreta | mês |
| `mes_nome` | Nome textual do mês | qualitativa ordinal | — |
| `Município` | Município polo analisado | qualitativa nominal | — |
| `Valor US$ FOB` | Soma do valor financeiro do período (FOB) | quantitativa contínua | US$ |
| `Quilograma Líquido` | Soma do peso líquido movimentado no período | quantitativa contínua | kg |

---

### 2.5. Principais Rotas Comerciais (`df_top_rotas.csv`)
Tabela de classificação das rotas (combinação de país e produto) mais representativas por fluxo e município.

| Variável | Descrição | Tipo | Unidade |
| :--- | :--- | :--- | :--- |
| `Fluxo` | Sentido da operação comercial (`Exportação` ou `Importação`) | qualitativa nominal | — |
| `Município` | Município polo de origem/destino | qualitativa nominal | — |
| `ranking` | Posição ordinal no ranking da rota comercial | quantitativa discreta | posição |
| `País` | País parceiro comercial da rota | qualitativa nominal | — |
| `Código SH4` | Código do Sistema Harmonizado de 4 dígitos | qualitativa nominal | — |
| `Descrição SH4` | Descrição oficial da mercadoria | qualitativa nominal | — |
| `volume_total_kg` | Volume total acumulado em quilogramas | quantitativa contínua | kg |
| `valor_total_fob` | Valor financeiro total acumulado (FOB) | quantitativa contínua | US$ |
| `participacao_pct` | Participação percentual individual da rota no fluxo/município | quantitativa contínua | % |
| `participacao_acumulada_pct` | Participação percentual acumulada (Curva de Pareto) | quantitativa contínua | % |

---

### 2.6. Séries Temporais das Rotas Líderes (`df_sazonal_lideres.csv`)
Dados temporais mensais focados exclusivamente nas rotas líderes (Top 1 de cada fluxo e município).

| Variável | Descrição | Tipo | Unidade |
| :--- | :--- | :--- | :--- |
| `Fluxo` | Sentido da operação comercial (`Exportação` ou `Importação`) | qualitativa nominal | — |
| `Município` | Município polo analisado | qualitativa nominal | — |
| `País` | País parceiro da rota líder | qualitativa nominal | — |
| `Código SH4` | Código do produto da rota líder | qualitativa nominal | — |
| `Descrição SH4` | Descrição do produto da rota líder | qualitativa nominal | — |
| `Ano` | Ano de registro | quantitativa discreta | ano |
| `mes_numero` | Número ordinal do mês (1 a 12) | quantitativa discreta | mês |
| `mes_nome` | Nome textual do mês | qualitativa ordinal | — |
| `Valor US$ FOB` | Valor financeiro mensal da rota líder (FOB) | quantitativa contínua | US$ |
| `Quilograma Líquido` | Peso líquido mensal da rota líder | quantitativa contínua | kg |

---

### 2.7. Transações Detalhadas das Rotas Líderes (`df_transacional_lideres.csv`)
Registros transacionais completos das rotas classificadas como Top 1.

| Variável | Descrição | Tipo | Unidade |
| :--- | :--- | :--- | :--- |
| `Fluxo` | Sentido da operação comercial (`Exportação` ou `Importação`) | qualitativa nominal | — |
| `Ano` | Ano de registro da operação comercial | quantitativa discreta | ano |
| `mes_numero` | Número ordinal do mês (1 a 12) | quantitativa discreta | mês |
| `mes_nome` | Nome textual do mês | qualitativa ordinal | — |
| `Município` | Município polo analisado | qualitativa nominal | — |
| `País` | País parceiro da rota líder | qualitativa nominal | — |
| `Código SH4` | Código identificador do produto | qualitativa nominal | — |
| `Valor US$ FOB` | Valor financeiro da transação (FOB) | quantitativa contínua | US$ |
| `Quilograma Líquido` | Peso líquido da mercadoria | quantitativa contínua | kg |
| `Descrição SH4` | Descrição textual oficial da mercadoria | qualitativa nominal | — |
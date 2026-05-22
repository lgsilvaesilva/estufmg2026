library(officer)
library(magick)

`%||%` <- function(x, y) if (is.null(x)) y else x

script_path <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NA_character_)
root <- if (!is.na(script_path)) normalizePath(file.path(dirname(script_path), "..")) else getwd()
if (!dir.exists(file.path(root, "img"))) root <- getwd()

asset_dir <- file.path(root, "pptx_assets")
dir.create(asset_dir, showWarnings = FALSE)

out_file <- file.path(root, "todos_os_caminhos_moderno.pptx")

W <- 13.333
H <- 7.5

COL <- list(
  ink = "#1A1917",
  night = "#101820",
  paper = "#F7F8FA",
  muted = "#D7DCE2",
  salt = "#61ACF0",
  fat = "#F0A561",
  acid = "#CBD20A",
  heat = "#E74A2F",
  blue2 = "#2F6FA3"
)

img <- function(name) file.path(root, "img", name)

safe_name <- function(path) {
  gsub("[^A-Za-z0-9_-]+", "_", tools::file_path_sans_ext(basename(path)))
}

cover_image <- function(src, width_in, height_in, darken = 0, gravity = "center") {
  target <- file.path(
    asset_dir,
    sprintf(
      "cover_%s_%sx%s_d%s.png",
      safe_name(src),
      round(width_in * 100),
      round(height_in * 100),
      round(darken * 100)
    )
  )
  if (!file.exists(target)) {
    px_w <- max(200, round(width_in * 220))
    px_h <- max(200, round(height_in * 220))
    im <- image_read(src)
    im <- image_resize(im, paste0(px_w, "x", px_h, "^"))
    im <- image_extent(im, paste0(px_w, "x", px_h), gravity = gravity)
    if (darken > 0) {
      im <- image_colorize(im, opacity = round(darken * 100), color = COL$night)
    }
    image_write(im, target, format = "png")
  }
  target
}

contain_image <- function(src, max_w, max_h, bg = "transparent") {
  target <- file.path(
    asset_dir,
    sprintf("contain_%s_%sx%s.png", safe_name(src), round(max_w * 100), round(max_h * 100))
  )
  if (!file.exists(target)) {
    px_w <- max(200, round(max_w * 220))
    px_h <- max(200, round(max_h * 220))
    im <- image_read(src)
    im <- image_resize(im, paste0(px_w, "x", px_h))
    im <- image_extent(im, paste0(px_w, "x", px_h), gravity = "center", color = bg)
    image_write(im, target, format = "png")
  }
  target
}

add_box <- function(p, x, y, w, h, fill = COL$night, line = fill) {
  ph_with(
    p,
    "",
    location = ph_location(x, y, w, h, bg = fill)
  )
}

add_image_fit <- function(p, path, x, y, w, h, crop = FALSE, darken = 0, gravity = "center") {
  src <- if (crop) cover_image(path, w, h, darken = darken, gravity = gravity) else contain_image(path, w, h)
  ph_with(p, external_img(src, width = w, height = h), location = ph_location(x, y, w, h))
}

txt_prop <- function(size = 24, color = COL$paper, bold = FALSE, italic = FALSE) {
  fp_text(font.family = "Aptos", font.size = size, color = color, bold = bold, italic = italic)
}

add_rich_text <- function(p, items, x, y, w, h, align = "left", spacing = 1.05) {
  pars <- lapply(items, function(it) {
    fpar(
      ftext(it$text, txt_prop(it$size %||% 22, it$color %||% COL$paper, it$bold %||% FALSE, it$italic %||% FALSE)),
      fp_p = fp_par(text.align = align, line_spacing = spacing)
    )
  })
  ph_with(p, do.call(block_list, pars), location = ph_location(x, y, w, h))
}

add_title <- function(p, title, subtitle = NULL, eyebrow = NULL, x = 0.72, y = 0.55, w = 11.9) {
  items <- list()
  if (!is.null(eyebrow)) {
    items <- c(items, list(list(text = toupper(eyebrow), size = 12, color = COL$acid, bold = TRUE)))
  }
  items <- c(items, list(list(text = title, size = 34, color = COL$paper, bold = TRUE)))
  if (!is.null(subtitle)) {
    items <- c(items, list(list(text = subtitle, size = 16, color = COL$muted)))
  }
  add_rich_text(p, items, x, y, w, ifelse(is.null(subtitle), 1.0, 1.6))
}

add_bullets <- function(p, bullets, x, y, w, h, color = COL$paper, size = 19, accent = COL$salt) {
  items <- lapply(bullets, function(b) {
    list(text = paste0("• ", b), size = size, color = color)
  })
  p <- add_rich_text(p, items, x, y, w, h, spacing = 1.18)
  p <- add_box(p, x - 0.16, y + 0.08, 0.05, min(0.52, h), fill = accent, line = accent)
  p
}

add_kicker <- function(p, text, x, y, w, color = COL$acid) {
  add_rich_text(p, list(list(text = text, size = 13, color = color, bold = TRUE)), x, y, w, 0.35)
}

add_footer <- function(p, section = NULL) {
  p <- add_box(p, 0, 7.22, W, 0.28, fill = COL$ink, line = COL$ink)
  label <- "lgsilvaesilva.github.io"
  if (!is.null(section)) label <- paste0(label, "  ·  ", section)
  add_rich_text(p, list(list(text = label, size = 8.5, color = COL$muted)), 0.35, 7.25, 6, 0.18)
}

add_slide_base <- function(p, bg = COL$night, section = NULL) {
  p <- add_slide(p, layout = "Blank", master = "Office Theme")
  p <- add_box(p, 0, 0, W, H, fill = bg, line = bg)
  add_footer(p, section)
}

add_metric_card <- function(p, x, y, w, h, number, label, accent = COL$salt) {
  p <- add_box(p, x, y, w, h, fill = "#202A35", line = "#202A35")
  p <- add_box(p, x, y, 0.08, h, fill = accent, line = accent)
  p <- add_rich_text(
    p,
    list(
      list(text = number, size = 28, color = accent, bold = TRUE),
      list(text = label, size = 12, color = COL$muted)
    ),
    x + 0.23, y + 0.18, w - 0.35, h - 0.22
  )
  p
}

add_two_columns <- function(p, left_title, left_items, right_title, right_items, section = NULL) {
  p <- add_slide_base(p, COL$night, section)
  p <- add_title(p, "Hard skills + soft skills", "O repertório técnico cresce junto com a forma de trabalhar.")
  p <- add_box(p, 0.72, 2.0, 5.65, 4.65, fill = "#202A35", line = "#202A35")
  p <- add_box(p, 6.68, 2.0, 5.65, 4.65, fill = "#202A35", line = "#202A35")
  p <- add_rich_text(p, list(list(text = left_title, size = 21, color = COL$heat, bold = TRUE)), 1.05, 2.28, 4.9, 0.45)
  p <- add_bullets(p, left_items, 1.1, 2.95, 4.9, 3.2, size = 16.5, accent = COL$heat)
  p <- add_rich_text(p, list(list(text = right_title, size = 21, color = COL$salt, bold = TRUE)), 7.0, 2.28, 4.9, 0.45)
  p <- add_bullets(p, right_items, 7.05, 2.95, 4.9, 3.2, size = 16.5, accent = COL$salt)
  p
}

new_deck <- function() {
  p <- read_pptx()
  layout <- slide_size(p)
  p
}

p <- new_deck()

# 1. Title
p <- add_slide_base(p, COL$ink)
p <- add_image_fit(p, img("rome_map.png"), 0, 0, W, H, crop = TRUE, darken = 0.45)
p <- add_rich_text(
  p,
  list(
    list(text = "Todos os caminhos me levaram a Roma", size = 43, color = COL$paper, bold = TRUE),
    list(text = "Passeio aleatório de um Estatístico até a ONU/FAO", size = 21, color = COL$acid),
    list(text = "Luís Silva e Silva · UFMG · Maio 2025", size = 14, color = COL$muted)
  ),
  0.8, 1.05, 8.8, 2.3
)
p <- add_rich_text(
  p,
  list(list(text = "Opiniões expressas em caráter pessoal.", size = 9.5, color = COL$muted, italic = TRUE)),
  0.8, 6.85, 6.2, 0.35
)

# 2. Find me
p <- add_slide_base(p, COL$paper)
p <- add_image_fit(p, img("myphoto.png"), 8.7, 1.0, 2.7, 2.7, crop = TRUE)
p <- add_rich_text(
  p,
  list(
    list(text = "Luís Silva e Silva", size = 34, color = COL$ink, bold = TRUE),
    list(text = "Estatístico · R · Visualização · Sistemas de dados", size = 17, color = COL$blue2),
    list(text = "@lgsilvaesilva · github.com/lgsilvaesilva · linkedin.com/in/lgsilvaesilva", size = 15, color = COL$ink)
  ),
  0.9, 1.15, 7.5, 2.4
)
p <- add_box(p, 0.9, 4.25, 10.7, 1.25, fill = "#E8EEF4", line = "#E8EEF4")
p <- add_rich_text(
  p,
  list(list(text = "Uma trajetória sobre aprender estatística colocando a mão nos problemas: saúde pública, texto, notícias, alimentos e decisões internacionais.", size = 20, color = COL$ink)),
  1.2, 4.52, 10.0, 0.9
)

# 3. Tukey
p <- add_slide_base(p, COL$night, "por que estatística?")
p <- add_image_fit(p, img("toolbox.jpeg"), 0, 0, W, H, crop = TRUE, darken = 0.72)
p <- add_rich_text(
  p,
  list(
    list(text = "\"The best thing about being a statistician is that you get to play in everyone's backyard.\"", size = 34, color = COL$paper, bold = TRUE),
    list(text = "John Tukey", size = 18, color = COL$acid)
  ),
  0.9, 1.3, 8.0, 2.4
)
p <- add_image_fit(p, img("John_Tukey.jpeg"), 9.4, 1.55, 2.55, 3.05, crop = TRUE)

# 4. Roadmap
p <- add_slide_base(p, COL$night, "trajetória")
p <- add_image_fit(p, img("maps.jpg"), 0, 0, W, H, crop = TRUE, darken = 0.62)
p <- add_title(p, "Passeio aleatório de um Estatístico", "Quatro estações, uma pergunta recorrente: como transformar dados em decisão?")
stops <- data.frame(
  x = c(1.2, 4.2, 7.2, 10.2),
  label = c("Graduação", "Mestrado", "Doutorado", "FAO"),
  years = c("2007-2010", "Arauca", "SUS · COWORDS · SFU", "DataLab")
)
p <- add_box(p, 1.15, 4.35, 10.5, 0.05, fill = COL$acid, line = COL$acid)
for (i in seq_len(nrow(stops))) {
  p <- add_box(p, stops$x[i], 4.0, 0.52, 0.52, fill = COL$fat, line = COL$fat)
  p <- add_rich_text(p, list(list(text = stops$label[i], size = 18, color = COL$paper, bold = TRUE), list(text = stops$years[i], size = 11.5, color = COL$muted)), stops$x[i] - 0.42, 4.75, 2.35, 0.75, align = "center")
}

# 5. Graduação
p <- add_slide_base(p, COL$paper, "graduação")
p <- add_title(p, "Graduação: matemática, código e R", "O começo foi uma mistura de cálculo, programação e descoberta de ferramentas.", y = 0.45)
p <- add_bullets(
  p,
  c("Cálculo IV, probabilidade e muita base matemática", "Disciplinas do DCC: estruturas de dados, filas, programação linear", "Estágio no CAEd e monitoria em probabilidade", "Conheço → odeio → amo → R"),
  0.95, 2.0, 5.6, 3.4,
  color = COL$ink,
  accent = COL$heat
)
p <- add_image_fit(p, img("graduacao.svg"), 7.05, 1.45, 4.5, 3.3)

# 6. Grad skills
p <- add_two_columns(
  p,
  "Hard skills",
  c("Manipulação de grandes bases", "Análise de dados", "SPSS, TRI e multivariada", "R e probabilidade"),
  "Soft skills",
  c("Relacionamento profissional", "Hierarquia institucional", "Desenvolvimento emocional", "Compartilhar conhecimento"),
  "graduação"
)

# 7. Mestrado problem
p <- add_slide_base(p, COL$night, "mestrado")
p <- add_image_fit(p, img("ambulancia02.jpeg"), 0, 0, W, H, crop = TRUE, darken = 0.58)
p <- add_rich_text(
  p,
  list(
    list(text = "Mestrado: Plataforma Arauca", size = 21, color = COL$fat, bold = TRUE),
    list(text = "Qual a distância que um indivíduo deve percorrer até alcançar atendimento médico?", size = 35, color = COL$paper, bold = TRUE)
  ),
  6.05, 0.95, 6.2, 3.4
)

# 8. Arauca
p <- add_slide_base(p, COL$paper, "mestrado")
p <- add_title(p, "Mapa de carência médica", "Dados georreferenciados para enxergar acesso à saúde.", y = 0.45)
p <- add_bullets(
  p,
  c("Medicina intensiva em Minas Gerais", "Todos os municípios do Brasil (~5570)", "Todas as especialidades médicas", "Integração entre R e Google Maps"),
  0.8, 1.8, 4.8, 3.2,
  color = COL$ink,
  accent = COL$acid
)
p <- add_image_fit(p, img("mgcarencia03.png"), 6.0, 1.45, 6.2, 3.25)
p <- add_image_fit(p, img("tese.png"), 9.65, 4.35, 1.6, 2.25)
p <- add_image_fit(p, img("premio.png"), 6.4, 4.45, 2.25, 1.7)

# 9. Masters lessons
p <- add_two_columns(
  p,
  "Hard skills",
  c("Método ágil de desenvolvimento", "Análise de dados georreferenciados", "Java, JavaScript, HTML, SVG", "Pacotes em R e teoria estatística"),
  "Soft skills",
  c("Empatia", "Resiliência", "Escrita", "Colaboração e compartilhamento"),
  "mestrado"
)

# 10. Doutorado opening
p <- add_slide_base(p, COL$night, "doutorado")
p <- add_image_fit(p, img("phd.jpeg"), 0, 0, W, H, crop = TRUE, darken = 0.62)
p <- add_title(p, "Doutorado", "Programa SUS, InfoSAS, COWORDS e uma temporada na Simon Fraser University.", y = 0.75)
p <- add_bullets(
  p,
  c("Origem, destino e tratamento de pacientes no SUS", "Detecção de anomalias em séries históricas", "Visualização probabilística de texto", "Pesquisa, inglês e abertura internacional"),
  0.95, 2.55, 6.6, 3.0,
  accent = COL$salt
)

# 11. Programa SUS
p <- add_slide_base(p, COL$paper, "doutorado")
p <- add_title(p, "Programa SUS", "Shiny para apoiar perguntas operacionais do Ministério da Saúde.", y = 0.45)
p <- add_bullets(
  p,
  c("Qual a origem dos pacientes?", "Quais tratamentos são realizados?", "Para onde vão os pacientes residentes da minha cidade?"),
  0.8, 1.75, 4.7, 2.6,
  color = COL$ink,
  accent = COL$salt
)
p <- add_image_fit(p, img("programasus.png"), 5.7, 1.25, 6.75, 3.85, crop = TRUE)
p <- add_image_fit(p, img("programasus-logo.png"), 0.95, 5.0, 2.5, 1.1)

# 12. InfoSAS
p <- add_slide_base(p, COL$night, "doutorado")
p <- add_title(p, "InfoSAS: anomalias no SUS", "Algoritmos, relatórios e visualização para uma rede nacional de saúde.")
p <- add_metric_card(p, 0.85, 2.05, 2.55, 1.2, "~5500", "cidades", COL$salt)
p <- add_metric_card(p, 3.65, 2.05, 2.55, 1.2, "6000", "estabelecimentos", COL$fat)
p <- add_metric_card(p, 0.85, 3.55, 2.55, 1.2, "5000", "procedimentos", COL$acid)
p <- add_metric_card(p, 3.65, 3.55, 2.55, 1.2, "15", "algoritmos", COL$heat)
p <- add_image_fit(p, img("infosasbrasil.png"), 7.0, 1.7, 5.15, 3.85)

# 13. Reports
p <- add_slide_base(p, COL$paper, "doutorado")
p <- add_title(p, "Relatórios: números que precisam circular", "De estatísticas robustas a uma folha A4 compreensível.", y = 0.35)
p <- add_bullets(
  p,
  c("Média, mediana, taxas e desvios", "MAD, percentis e janela móvel", "Bayes empírico", "Relatórios com síntese e rastreabilidade"),
  0.75, 1.65, 4.2, 2.8,
  color = COL$ink,
  accent = COL$heat
)
p <- add_image_fit(p, img("relatorio-exemplo-pg1.png"), 5.45, 1.25, 2.2, 4.9)
p <- add_image_fit(p, img("relatorio-exemplo-pg2.png"), 7.85, 1.25, 2.2, 4.9)
p <- add_image_fit(p, img("relatorio-exemplo-pg3.png"), 10.25, 1.25, 2.2, 4.9)

# 14. COWORDS
p <- add_slide_base(p, COL$night, "doutorado")
p <- add_title(p, "COWORDS", "Um modelo probabilístico para visualizar sequências de nuvens de palavras.")
p <- add_bullets(
  p,
  c("Compactar sem sobrepor palavras", "Manter palavras recorrentes na mesma posição", "Transformar critérios visuais em uma distribuição de probabilidade"),
  0.85, 2.0, 5.5, 2.7,
  accent = COL$acid
)
p <- add_image_fit(p, img("wordcloud.png"), 6.5, 1.55, 5.8, 2.8)
p <- add_image_fit(p, img("cowords.png"), 9.3, 4.25, 1.55, 2.2)

# 15. SFU
p <- add_slide_base(p, COL$night, "sfu")
p <- add_image_fit(p, img("sfu-lago.jpeg"), 0, 0, W, H, crop = TRUE, darken = 0.55)
p <- add_title(p, "Simon Fraser University", "Quatro meses de pesquisa, desafios, inglês e uma nova porta.", y = 0.75)
p <- add_bullets(
  p,
  c("Muito aprendizado técnico e pessoal", "Abertura para colaboração internacional", "A tese encontrou outro ambiente de pesquisa"),
  0.95, 2.6, 6.2, 2.4,
  accent = COL$salt
)

# 16. PhD lessons
p <- add_two_columns(
  p,
  "Hard skills",
  c("C++ e computação paralela", "Análise de dados +++", "R +++", "Aprofundamento teórico", "Inglês"),
  "Soft skills",
  c("Pensamento organizado", "Comunicação", "Empatia e resiliência", "Colaboração global"),
  "doutorado"
)

# 17. FAO opening
p <- add_slide_base(p, COL$night, "FAO")
p <- add_image_fit(p, img("fao01.jpeg"), 0, 0, W, H, crop = TRUE, darken = 0.48)
p <- add_image_fit(p, img("fao-logo2.svg"), 0.9, 0.75, 4.2, 1.05)
p <- add_rich_text(
  p,
  list(
    list(text = "Da universidade para a ONU/FAO", size = 39, color = COL$paper, bold = TRUE),
    list(text = "A estatística entra quando decisões globais precisam de dados comparáveis, revisados e comunicáveis.", size = 19, color = COL$muted)
  ),
  0.95, 2.05, 8.2, 2.1
)

# 18. What FAO does
p <- add_slide_base(p, COL$paper, "FAO")
p <- add_title(p, "O que a FAO faz?", "Dados, conhecimento e coordenação para agricultura, alimentação e segurança alimentar.", y = 0.45)
p <- add_image_fit(p, img("oqueafaofaz.png"), 0.9, 1.55, 11.5, 4.35)

# 19. Statistician at FAO
p <- add_slide_base(p, COL$night, "FAO")
p <- add_image_fit(p, img("oqfao.jpeg"), 0, 0, W, H, crop = TRUE, darken = 0.63)
p <- add_title(p, "O que um Estatístico faz na FAO?", "Transforma dados heterogêneos em evidência confiável.", y = 0.75)
p <- add_bullets(
  p,
  c("Padronização de países, commodities e códigos internacionais", "Imputação de dados faltantes e correção de outliers", "Relatórios estatísticos e séries comparáveis", "Previsão de consumo de carne e estimação de insegurança alimentar"),
  6.55, 2.0, 5.6, 3.6,
  accent = COL$fat
)

# 20. First year product
p <- add_slide_base(p, COL$paper, "FAO")
p <- add_title(p, "Primeiro ano: meu principal produto", "Uma esteira de dados para fisheries integrada ao sistema da FAO.", y = 0.4)
p <- add_bullets(
  p,
  c("Obtenção dos dados", "Codificação dos países e commodities", "Tratamento de dados faltantes", "Tratamento de valores discrepantes", "Integração com o sistema da FAO"),
  0.8, 1.65, 4.7, 3.7,
  color = COL$ink,
  accent = COL$acid
)
p <- add_image_fit(p, img("app-fisheries.png"), 5.7, 1.35, 6.55, 3.35)

# 21. DataLab projects
p <- add_slide_base(p, COL$night, "FAO")
p <- add_title(p, "DataLab: texto, notícias e tendências", "Projetos que combinam mineração de texto, sentimento, monitoramento e comunicação.")
p <- add_metric_card(p, 0.85, 1.9, 2.6, 1.15, "~4000", "documentos de políticas", COL$acid)
p <- add_metric_card(p, 3.75, 1.9, 2.6, 1.15, "~200", "países", COL$salt)
p <- add_metric_card(p, 0.85, 3.25, 2.6, 1.15, "7", "idiomas", COL$fat)
p <- add_metric_card(p, 3.75, 3.25, 2.6, 1.15, "10M", "notícias", COL$heat)
p <- add_image_fit(p, img("topics-explorer.png"), 6.9, 1.55, 5.75, 2.45)
p <- add_image_fit(p, img("datalab-trends.png"), 6.9, 4.05, 5.75, 2.25)

# 22. DataLab webpage
p <- add_slide_base(p, COL$paper, "FAO")
p <- add_title(p, "DataLab webpage", "Uma vitrine para explorar produtos e evidências.", y = 0.35)
p <- add_image_fit(p, img("datalabpage.png"), 1.25, 1.25, 10.8, 5.05)
p <- add_rich_text(p, list(list(text = "fao.org/datalab", size = 16, color = COL$blue2, bold = TRUE)), 1.25, 6.35, 5, 0.35)

# 23. FAO lessons
p <- add_two_columns(
  p,
  "Hard skills",
  c("Google Cloud", "Solr DB / NoSQL", "Pipelines de dados", "Produtos analíticos em produção"),
  "Soft skills",
  c("Comunicação ++", "Resiliência +++", "Trabalhar sob pressão", "Colaboração global"),
  "FAO"
)

# 24. Synthesis
p <- add_slide_base(p, COL$night, "síntese")
p <- add_image_fit(p, img("brain02.png"), 8.0, 0.8, 4.4, 4.4)
p <- add_title(p, "Trajetória até aqui", "O caminho não foi linear, mas os padrões aparecem olhando para trás.", y = 0.65)
p <- add_bullets(
  p,
  c("Teoria estatística: probabilidade, inferência e modelagem", "Programação: lógica + R + prática", "Análise de dados: colocar a mão na massa", "Comunicação, compartilhar, resiliência, curiosidade e respeito"),
  0.9, 2.2, 7.0, 3.45,
  accent = COL$heat
)

# 25. Closing
p <- add_slide_base(p, COL$ink)
p <- add_image_fit(p, img("rome_map.png"), 0, 0, W, H, crop = TRUE, darken = 0.55)
p <- add_image_fit(p, img("myphoto.png"), 9.5, 0.9, 2.2, 2.2, crop = TRUE)
p <- add_rich_text(
  p,
  list(
    list(text = "Obrigado!", size = 48, color = COL$paper, bold = TRUE),
    list(text = "@lgsilvaesilva · github.com/lgsilvaesilva · lgsilvaesilva.github.io", size = 17, color = COL$acid),
    list(text = "Todos os caminhos me levaram a Roma.", size = 19, color = COL$muted)
  ),
  0.9, 1.35, 8.5, 2.55
)

print(p, target = out_file)
message("Created: ", out_file)

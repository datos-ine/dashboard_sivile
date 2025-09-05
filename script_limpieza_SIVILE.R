
# Cargar paquetes --------------------------------------------------------
pacman::p_load(
  rio,
  janitor,
  epikit,
  geoAr,
  tidyverse
)


# Cargar/limpiar datos provincias ----------------------------------------
## Códigos provincias
cod_prov <- show_arg_codes() |>
  # Cambiar formato id provincia
  mutate(codprov = parse_number(codprov)) |>

  # Filtrar totales
  filter(between(codprov, 1, 24))


# Cargar datos crudos SIVILE ---------------------------------------------
datos_raw <- read_delim(
  "raw/export_Sistema_de_Vigilancia_de_Lesiones_SIVILE_Completo_2024.txt"
)


# Limpiar datos SIVILE ---------------------------------------------------
datos <- datos_raw |>
  # Estandarizar nombres de columnas
  clean_names() |>
  rename_with(.fn = ~ str_remove_all(.x, "^[^_]*_")) |>

  # Renombrar columnas manualmente
  rename(
    prov_residencia_id = provincia_residencia_id,
    prov_lesion_id = provincia_lesion_id,
    sit_laboral_id = situacion_laboral_id,
    rel_vict_agresor = relacion_victima_agresor_id,
    SPG = score_practico_gravedad_id
  ) |>

  # Filtrar datos ausentes provincia lesión
  filter(between(prov_lesion_id, 1, 24)) |>

  # Filtrar datos ausentes sexo
  filter(between(sexo, "F", "M")) |>

  # Filtrar datos ausentes edad
  filter(!is.na(edad)) |>
  
  # Filtrar datos ausentes mecanismo
  filter(between(mecanismo_id, 1, 22)) |> 

  # Cambiar formato fechas
  mutate(across(.cols = contains("fecha"), .fns = ~ dmy(.x))) |>

  # Cambiar etiquetas sexo
  mutate(sexo = fct_relabel(sexo, ~ c("Femenino", "Masculino"))) |>

  # Añadir etiquetas provincia lesión
  left_join(cod_prov |> 
    select(prov_lesion_id = codprov,
    prov_lesion = name_iso)) |>

  # Crear variable para grupo edad ampliado
  mutate(
    grupo_edad = age_categories(
      edad,
      breakers = c(0, 2, 5, 10, 15, 25, 45, 65)
    ) |> 
      # Añadir etiqueta años
      paste("años"),
    .after = edad
  ) |> 

  # Recategorizar situación laboral
  mutate(
    sit_laboral_cat = case_when(
      between(sit_laboral_id, 1, 3) ~ "Trabajador",
      sit_laboral_id == 4 ~ "Desempleado",
      sit_laboral_id == 5 ~ "Estudiante",
      sit_laboral_id == 6 ~ "Ama de casa",
      sit_laboral_id == 7 ~ "Jubilado/pensionado",
      between(sit_laboral_id, 8, 9) ~ "Otro",
      .default = NA
    ),
    .after = sit_laboral_id
  ) |>

  # Recategorizar nivel educativo
  mutate(
    nivel_instruccion_cat = case_when(
      between(nivel_instruccion_id, 1, 2) ~ "Ninguno/primario incompleto",
      between(nivel_instruccion_id, 3, 4) ~ "Primario completo",
      between(nivel_instruccion_id, 5, 6) ~ "Secundario completo",
      nivel_instruccion_id == 7 ~ "Terciario/universitario completo",
      .default = NA
    ),
    .after = nivel_instruccion_id
  ) |>

  # Recategorizar mecanismo de lesión
  mutate(
    mecanismo_cat = case_when(
      mecanismo_id == 1 ~ "Transporte",
      mecanismo_id == 2 ~ "Envenenamiento/intoxicación",
      mecanismo_id == 3 ~ "Golpe",
      mecanismo_id == 4 ~ "Caída (mismo nivel)",
      mecanismo_id == 5 ~ "Caída (un nivel a otro)",
      mecanismo_id == 6 ~ "Objeto cortopunzante",
      mecanismo_id == 7 ~ "Arma de fuego",
      between(mecanismo_id, 8, 11) ~ "Quemadura",
      mecanismo_id == 12 ~ "Electricidad",
      mecanismo_id == 13 ~ "Cuerpo extraño",
      between(mecanismo_id, 14, 16) ~ "Asfixia/ahogamiento",
      mecanismo_id == 17 ~ "Animal/planta",
      mecanismo_id == 18 ~ "Mordedura de perro",
      mecanismo_id == 20 ~ "Aplastado/atrapado/apretado",
      mecanismo_id == 21 ~ "Agresión sexual",
      mecanismo_id %in% c(19, 22) ~ "Otro",
      .default = NA
    ),
    .after = mecanismo_id
  ) |>

  # Categorizar condición lesionado
  mutate(
    condicion_lesionado_cat = case_when(
      condicion_lesionado_id == 1 ~ "Peatón",
      condicion_lesionado_id == 2 ~ "Pasajero",
      condicion_lesionado_id == 3 ~ "Conductor",
      condicion_lesionado_id == 4 ~ "Otro",
      .default = NA
    ),
    .after = condicion_lesionado_id
  ) |>

  # Categorizar tipo de vehículo
  mutate(
    modo_transporte_cat = case_when(
      modo_transporte_id == 1 ~ "Automóvil",
      modo_transporte_id == 2 ~ "Camioneta/furgoneta",
      modo_transporte_id == 3 ~ "Omnibus",
      modo_transporte_id == 4 ~ "Camión",
      modo_transporte_id == 5 ~ "Bicicleta",
      modo_transporte_id == 6 ~ "Moto/ciclomotor",
      modo_transporte_id == 7 ~ "Tren",
      modo_transporte_id == 8 ~ "Tracción animal",
      modo_transporte_id == 9 ~ "Otro",
      .default = NA
    ),
    .after = modo_transporte_id
  ) |>

  # Categorizar contraparte
  mutate(
    contraparte_cat = case_when(
      contraparte_id == 1 ~ "Peatón",
      contraparte_id == 2 ~ "Animal",
      contraparte_id == 3 ~ "Automóvil",
      contraparte_id == 4 ~ "Camioneta/furgoneta",
      contraparte_id == 5 ~ "Omnibus",
      contraparte_id == 6 ~ "Camión",
      contraparte_id == 7 ~ "Bicicleta",
      contraparte_id == 8 ~ "Moto/ciclomotor",
      contraparte_id == 9 ~ "Tren",
      contraparte_id == 10 ~ "Tracción animal",
      contraparte_id == 11 ~ "Objeto fijo",
      contraparte_id == 12 ~ "Evento sin colisión",
      contraparte_id == 13 ~ "Otro",
      .default = NA
    ),
    .after = contraparte_id
  ) |>

  # Categorizar etiquetas lugar lesión
  mutate(
    lugar_lesion_cat = case_when(
      lugar_lesion_id == 1 ~ "Lugar de uso público/vía pública",
      lugar_lesion_id == 2 ~ "Vivienda",
      lugar_lesion_id == 3 ~ "Institución",
      lugar_lesion_id == 4 ~ "Otro",
      .default = NA
    ),
    .after = lugar_lesion_id
  ) |>

  # Categorizar intencionalidad
  mutate(
    intencionalidad_cat = case_when(
      intencionalidad_id == 1 ~ "Interpersonal",
      intencionalidad_id == 2 ~ "Autoinfligida",
      intencionalidad_id == 3 ~ "No intencional",
      between(intencionalidad_id, 4, 5) ~ "Otra",
      .default = NA
    ),
    .after = intencionalidad_id
  ) |>

  # Categorizar relación con agresor
  mutate(
    rel_vict_agresor = case_when(
      rel_vict_agresor == 1 ~ "Pareja/ex pareja",
      rel_vict_agresor == 2 ~ "Padres/padrastros",
      rel_vict_agresor == 3 ~ "Otro familiar",
      rel_vict_agresor == 4 ~ "Amigos/conocidos",
      rel_vict_agresor == 5 ~ "Extraño",
      rel_vict_agresor == 6 ~ "Otro",
      .default = NA
    )
  ) |>

  # Modificar etiquetas sexo agresor
  mutate(
    sexo_agresor = fct_relabel(
      sexo_agresor,
      ~ c("Femenino", "Masculino", "Múltiples agresores", "Desconocido")
    ),
  ) |>

  # Modificar etiquetas contexto agresión
  mutate(
    contexto_agresion_cat = case_when(
      contexto_agresion_id == 1 ~ "Violencia doméstica/intrafamiliar",
      contexto_agresion_id == 2 ~ "Robo/otros crímenes",
      contexto_agresion_id == 3 ~ "Riñas o peleas",
      contexto_agresion_id == 4 ~ "Otro",
      .default = NA
    ),
    .after = contexto_agresion_id
  ) |>

  # Modificar etiquetas SPG
  mutate(
    SPG = case_when(
      SPG == 1 ~ "Leve",
      SPG == 2 ~ "Moderado",
      SPG == 3 ~ "Grave"
    )
  ) |>

  # Categorizar egreso
  mutate(
    egreso_cat = case_when(
      egreso_id == 1 ~ "Ambulatorio/alta",
      egreso_id == 2 ~ "Internación área no crítica",
      egreso_id == 3 ~ "Internación área crítica",
      egreso_id == 4 ~ "Derivación a otra institución",
      egreso_id == 5 ~ "Alta voluntaria/fuga",
      egreso_id == 5 ~ "Fallecimiento",
      .default = NA
    ),
    .after = egreso_id
  ) |>

  # Modificar niveles columnas binarias
  mutate(across(
    .cols = c(cinturon_seguridad:asiento_ninos, intento_previo:naturaleza_otro),
    .fns = ~ case_when(
      .x %in% c("S", "SI") ~ 1,
      .x %in% c("N", "NO") ~ 0,
      .default = NA
    )
  )) |>

  # Seleccionar columnas
  select(
    codigo_institucion_id,
    fecha_consulta,
    sexo:nivel_instruccion_cat,
    fecha_lesion,
    prov_residencia_id,
    prov_lesion_id,
    prov_lesion,
    mecanismo_id,
    mecanismo_cat,
    condicion_lesionado_id:asiento_ninos,
    intencionalidad_id:naturaleza_otro,
    SPG,
    egreso_id,
    egreso_cat
  )


# Exportar datos limpios -------------------------------------------------
export(datos, file = "dashboard_sivile/datos_sivile_2024.csv")

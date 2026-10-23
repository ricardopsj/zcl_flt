# zcl_flt — Filtro dinámico

Constructor de cláusulas `WHERE` dinámicas a partir de condiciones por campo (valores sueltos o rangos `SELECT-OPTIONS`). Acumula condiciones con nombre y las combina en un string listo para usar en Open SQL dinámico o en sentencias ABAP (`LOOP ... WHERE (lv_where)`), apoyándose en el estándar `cl_shdb_seltab=>combine_seltabs`.

```abap
data(flt) = new zcl_flt( ).

flt->add_value( field = 'MATNR' low = lv_matnr ).                 " MATNR EQ lv_matnr
flt->add_value( field = 'WERKS' option = 'BT' low = '1000' high = '2000' ).
flt->add_range( field = 'ERDAT' range = s_erdat[] ).              " un SELECT-OPTIONS completo

select * from mara into table @data(lt_mara)
 where (flt->get_where_string_sql( )).
```

## Instalación

Importar con [abapGit](https://abapgit.org). Requiere ABAP 7.40 o superior con `CL_SHDB_SELTAB` / `IF_SHDB_DEF` disponibles (NW 7.40 SP5+). Sin dependencias de otros paquetes Z.

## API

### Agregar condiciones

| Método | Descripción |
|---|---|
| `add_value( field sign option low high )` | Agrega una condición simple. `sign` por defecto `'I'`, `option` por defecto `'EQ'`. Acepta alias de operadores: `=`, `<>`, `>`, `>=`, `<`, `<=` (se normalizan a `EQ`/`NE`/`GT`/`GE`/`LT`/`LE`). El tipo del rango se construye en runtime (RTTS) a partir del tipo de `low`/`high`, así que conserva conversion exits y longitudes |
| `add_range( field range )` | Agrega un rango completo (`RANGE OF` / `SELECT-OPTIONS`) |
| `add_ref_value( field sign option low high )` | Variante de `add_value` con `low`/`high` como `REF TO DATA` |
| `add_ref_range( field ref_range )` | Variante de `add_range` por referencia |
| `st_range_from_value( sign option low high changing range )` | (estática) Agrega una línea a cualquier tabla-rango tipada, sin conocer su tipo en tiempo de diseño |

### Obtener el WHERE

| Método | Salida |
|---|---|
| `get_where_string( )` | WHERE combinado en sintaxis de `combine_seltabs` (operadores `EQ`, `BT`, `LIKE`…), con correcciones de espaciado y limpieza de timestamps |
| `get_where_string_abap( )` | Variante para sentencias ABAP: convierte `LIKE '...%'` en `CP '...*'` y `NOT LIKE` en `NP` |
| `get_where_string_sql( )` | Variante para Open SQL: `EQ`→`=`, `NE`→`<>`, `BT`→`between`, `GE`→`>=`, etc. |

Las condiciones de distintos campos se combinan con `AND`; las líneas de un mismo rango siguen la semántica estándar de rangos (incluyentes con `OR`, excluyentes negadas).

### Gestión del estado

| Método | Descripción |
|---|---|
| `remove( field )` | Elimina todas las condiciones de un campo |
| `refresh( )` | Elimina todas las condiciones |
| `push_cond( )` / `pop_cond( )` | Guarda/restaura el juego de condiciones en una pila; útil para variar temporalmente un filtro (p. ej. agregar una condición extra para una consulta puntual y volver al estado anterior) |

```abap
flt->add_value( field = 'MATNR' low = lv_matnr ).
flt->push_cond( ).
flt->add_value( field = 'WERKS' low = lv_werks ).
" ... consulta con MATNR + WERKS ...
flt->pop_cond( ).            " vuelve a quedar solo MATNR
```

## Tests

Incluye una suite ABAP Unit (`ltcl_test`) que cubre: condición `EQ` y `BT`, rangos, múltiples campos, `remove`, `refresh`, `push`/`pop`, generación SQL y ABAP (`CP`), `st_range_from_value` y la normalización de operadores alias.

## Problemas conocidos

- **WHERE vacío ante errores.** `get_where_string( )` descarta la excepción de `combine_seltabs` y devuelve un string vacío. Un WHERE dinámico vacío no filtra nada: la consulta lee todas las filas.
- **Varios `LIKE` en el mismo WHERE.** La expresión regular de `get_where_string_abap( )` es codiciosa; con dos condiciones `LIKE` en el mismo WHERE corrompe la cadena. Tampoco traduce el comodín `_` a `+`.
- **Condición sin valores.** `add_value` y `add_ref_value` terminan en dump si no reciben ni `low` ni `high`.
- **`pop_cond( )` con la pila vacía** deja el filtro sin condiciones.

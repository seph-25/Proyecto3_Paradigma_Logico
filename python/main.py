#!/usr/bin/env python3
import tkinter as tk
from tkinter import ttk, messagebox
from pyswip import Prolog
import os
from datetime import datetime

# ============================================================================
# CLASE PRINCIPAL
# ============================================================================

class MenuSaludableApp:
    def __init__(self, root):
        self.root = root
        self.root.title("Menú Saludable Inteligente")
        self.root.geometry("900x700")

        # Initialize Prolog
        self.prolog = Prolog()

        # Load Prolog knowledge base
        prolog_file = os.path.join(os.path.dirname(__file__), "..", "prolog", "menu_saludable.pl")
        prolog_file = os.path.abspath(prolog_file)

        if not os.path.exists(prolog_file):
            messagebox.showerror("Error", f"No se encontró el archivo Prolog: {prolog_file}")
            root.destroy()
            return

        self.prolog.consult(prolog_file)

        # Historial de aceptaciones/rechazos
        self.historial = []

        # Variables de filtros
        self.var_vegetariano = tk.BooleanVar(value=False)
        self.var_con_postre = tk.BooleanVar(value=True)
        self.var_tipo_carne = tk.StringVar(value="Todas")
        self.var_cal_min = tk.StringVar(value="")
        self.var_cal_max = tk.StringVar(value="")
        
        # Variables para ingredientes
        self.ingredientes_incluir = set()
        self.ingredientes_excluir = set()
        self.ingredientes_data = {}

        # Menús generados
        self.menus_actuales = []

        self.cargar_ingredientes()
        self.create_widgets()

    def cargar_ingredientes(self):
        """Carga todos los ingredientes disponibles desde Prolog y los categoriza"""
        try:
            self.ingredientes_data = {
                "Entradas": [],
                "Carbohidratos": [],
                "Proteínas": [],
                "Vegetales": [],
                "Postres": []
            }
            
            # Cargar entradas
            for result in self.prolog.query("entrada(Nombre, Calorias, Vegetariano)"):
                self.ingredientes_data["Entradas"].append({
                    "nombre": str(result["Nombre"]),
                    "calorias": result["Calorias"],
                    "vegetariano": result["Vegetariano"]
                })
            
            # Cargar carbohidratos
            for result in self.prolog.query("carbohidrato(Nombre, Calorias, Vegetariano)"):
                self.ingredientes_data["Carbohidratos"].append({
                    "nombre": str(result["Nombre"]),
                    "calorias": result["Calorias"],
                    "vegetariano": result["Vegetariano"]
                })
            
            # Cargar carnes/proteínas
            for result in self.prolog.query("carne(Nombre, Calorias, Tipo, Vegetariano)"):
                self.ingredientes_data["Proteínas"].append({
                    "nombre": str(result["Nombre"]),
                    "calorias": result["Calorias"],
                    "tipo": str(result["Tipo"]),
                    "vegetariano": result["Vegetariano"]
                })
            
            # Cargar vegetales
            for result in self.prolog.query("vegetal(Nombre, Calorias, Vegetariano)"):
                self.ingredientes_data["Vegetales"].append({
                    "nombre": str(result["Nombre"]),
                    "calorias": result["Calorias"],
                    "vegetariano": result["Vegetariano"]
                })
            
            # Cargar postres
            for result in self.prolog.query("postre(Nombre, Calorias, Vegetariano)"):
                self.ingredientes_data["Postres"].append({
                    "nombre": str(result["Nombre"]),
                    "calorias": result["Calorias"],
                    "vegetariano": result["Vegetariano"]
                })
                
        except Exception as e:
            print(f"Error cargando ingredientes: {e}")
            messagebox.showerror("Error", f"No se pudieron cargar los ingredientes: {e}")

    def create_widgets(self):
        # ===== FRAME DE FILTROS =====
        frame_filtros = ttk.LabelFrame(self.root, text="Filtros", padding=10)
        frame_filtros.pack(fill="x", padx=10, pady=5)

        # Fila 1: Vegetariano y Postre
        fila1 = ttk.Frame(frame_filtros)
        fila1.pack(fill="x", pady=5)

        ttk.Checkbutton(fila1, text="Solo Vegetariano",
                       variable=self.var_vegetariano,
                       command=self.on_toggle_vegetariano).pack(side="left", padx=10)

        ttk.Checkbutton(fila1, text="Con Postre",
                       variable=self.var_con_postre,
                       command=self.generar_menus).pack(side="left", padx=10)

        # Fila 2: Tipo de Carne
        fila2 = ttk.Frame(frame_filtros)
        fila2.pack(fill="x", pady=5)

        ttk.Label(fila2, text="Tipo de Carne:").pack(side="left", padx=5)
        tipos_carne = ["Todas", "Pollo", "Res", "Pescado", "Cerdo", "Vegetariano"]
        combo_carne = ttk.Combobox(fila2, textvariable=self.var_tipo_carne,
                                   values=tipos_carne, state="readonly", width=15)
        combo_carne.pack(side="left", padx=5)
        # Guardar como atributo para poder activarlo/desactivarlo cuando se marque "Solo Vegetariano"
        self.combo_carne = combo_carne
        combo_carne.bind("<<ComboboxSelected>>", lambda e: self.generar_menus())

        # Fila 3: Calorías
        fila3 = ttk.Frame(frame_filtros)
        fila3.pack(fill="x", pady=5)

        ttk.Label(fila3, text="Calorías Mínimas:").pack(side="left", padx=5)
        entry_min = ttk.Entry(fila3, textvariable=self.var_cal_min, width=10)
        entry_min.pack(side="left", padx=5)
        entry_min.bind("<KeyRelease>", lambda e: self.generar_menus())

        ttk.Label(fila3, text="Máximas:").pack(side="left", padx=5)
        entry_max = ttk.Entry(fila3, textvariable=self.var_cal_max, width=10)
        entry_max.pack(side="left", padx=5)
        entry_max.bind("<KeyRelease>", lambda e: self.generar_menus())

        ttk.Button(fila3, text="Regenerar Menús",
                  command=self.generar_menus).pack(side="left", padx=20)

        # ===== FRAME DE MENÚS =====
        frame_menus = ttk.LabelFrame(self.root, text="Menús Sugeridos", padding=10)
        frame_menus.pack(fill="both", expand=True, padx=10, pady=5)

        # Contenedor con scroll
        self.canvas = tk.Canvas(frame_menus)
        scrollbar = ttk.Scrollbar(frame_menus, orient="vertical", command=self.canvas.yview)
        self.frame_menus_scrollable = ttk.Frame(self.canvas)

        self.frame_menus_scrollable.bind(
            "<Configure>",
            lambda e: self.canvas.configure(scrollregion=self.canvas.bbox("all"))
        )

        self.canvas.create_window((0, 0), window=self.frame_menus_scrollable, anchor="nw")
        self.canvas.configure(yscrollcommand=scrollbar.set)

        self.canvas.pack(side="left", fill="both", expand=True)
        scrollbar.pack(side="right", fill="y")

        # Vincular evento de redimensionamiento para reorganizar tarjetas
        self.canvas.bind("<Configure>", self.reorganizar_tarjetas)

        # ===== FRAME DE SELECCIÓN DE INGREDIENTES =====
        frame_ingredientes = ttk.LabelFrame(self.root, text="Seleccionar Ingredientes", padding=10)
        frame_ingredientes.pack(fill="both", expand=False, padx=10, pady=5)

        # Crear notebook para organizar por categorías
        notebook_ingredientes = ttk.Notebook(frame_ingredientes)
        notebook_ingredientes.pack(fill="both", expand=True)

        self.checkboxes_ingredientes = {}
        self.vars_ingredientes = {}

        for categoria, ingredientes in self.ingredientes_data.items():
            # Frame para cada categoría
            frame_categoria = ttk.Frame(notebook_ingredientes)
            notebook_ingredientes.add(frame_categoria, text=categoria)

            # Frame con scroll para la lista de ingredientes
            canvas_ingredientes = tk.Canvas(frame_categoria, height=150)
            scrollbar_ingredientes = ttk.Scrollbar(frame_categoria, orient="vertical", command=canvas_ingredientes.yview)
            frame_scroll_ingredientes = ttk.Frame(canvas_ingredientes)

            frame_scroll_ingredientes.bind(
                "<Configure>",
                lambda e: canvas_ingredientes.configure(scrollregion=canvas_ingredientes.bbox("all"))
            )

            canvas_ingredientes.create_window((0, 0), window=frame_scroll_ingredientes, anchor="nw")
            canvas_ingredientes.configure(yscrollcommand=scrollbar_ingredientes.set)

            canvas_ingredientes.pack(side="left", fill="both", expand=True)
            scrollbar_ingredientes.pack(side="right", fill="y")

            # Controles para la categoría
            frame_controles = ttk.Frame(frame_categoria)
            frame_controles.pack(fill="x", pady=5)

            ttk.Button(frame_controles, text=f"Incluir Todos ({categoria})",
                      command=lambda cat=categoria: self.incluir_todos_categoria(cat)).pack(side="left", padx=5)
            ttk.Button(frame_controles, text=f"Excluir Todos ({categoria})",
                      command=lambda cat=categoria: self.excluir_todos_categoria(cat)).pack(side="left", padx=5)
            ttk.Button(frame_controles, text=f"Limpiar ({categoria})",
                      command=lambda cat=categoria: self.limpiar_categoria(cat)).pack(side="left", padx=5)

            # Crear checkboxes para cada ingrediente
            self.vars_ingredientes[categoria] = {}
            
            for ingrediente in ingredientes:
                nombre = ingrediente["nombre"]
                calorias = ingrediente["calorias"]
                
                frame_ingrediente = ttk.Frame(frame_scroll_ingredientes)
                frame_ingrediente.pack(fill="x", pady=1)

                # Variables para incluir y excluir
                var_incluir = tk.BooleanVar(value=False)
                var_excluir = tk.BooleanVar(value=False)
                
                self.vars_ingredientes[categoria][nombre] = {
                    "incluir": var_incluir,
                    "excluir": var_excluir
                }

                # Checkbox para incluir
                check_incluir = ttk.Checkbutton(frame_ingrediente, text="Inc",
                                              variable=var_incluir,
                                              command=lambda n=nombre, cat=categoria: self.on_incluir_ingrediente(cat, n))
                check_incluir.pack(side="left", padx=2)

                # Checkbox para excluir
                check_excluir = ttk.Checkbutton(frame_ingrediente, text="Exc",
                                              variable=var_excluir,
                                              command=lambda n=nombre, cat=categoria: self.on_excluir_ingrediente(cat, n))
                check_excluir.pack(side="left", padx=2)

                # Label con nombre y calorías
                ttk.Label(frame_ingrediente, text=f"{nombre} ({calorias} cal)",
                         width=25).pack(side="left", padx=5)

        # ===== FRAME DE ESTADÍSTICAS =====
        frame_stats = ttk.LabelFrame(self.root, text="Estadísticas", padding=10)
        frame_stats.pack(fill="x", padx=10, pady=5)

        self.label_stats = ttk.Label(frame_stats, text="Aceptados: 0 | Rechazados: 0 | Total: 0")
        self.label_stats.pack()

        # Generar menús iniciales
        # Sincronizar estado del combobox con el filtro vegetariano al inicio
        self.on_toggle_vegetariano()
        self.generar_menus()

    def on_toggle_vegetariano(self):
        """Habilita/deshabilita el combobox de tipo de carne cuando se selecciona "Solo Vegetariano".

        - Si se marca vegetariano: fija el valor de tipo_carne a 'Vegetariano' y desactiva el combobox.
        - Si se desmarca: vuelve el combobox a estado 'readonly' (reactivándolo) y mantiene el valor previo si corresponde.
        Finalmente regenera los menús.
        """
        try:
            if self.var_vegetariano.get():
                # Forzar que el filtro de tipo de carne refleje vegetariano y deshabilitar selector
                self.var_tipo_carne.set("Vegetariano")
                # Si el widget fue creado, deshabilitarlo para evitar cambios
                if hasattr(self, 'combo_carne'):
                    self.combo_carne.config(state="disabled")
            else:
                # Reactivar selector de carnes
                if hasattr(self, 'combo_carne'):
                    self.combo_carne.config(state="readonly")
                    self.combo_carne.set("Todas")

        except Exception as e:
            print(f"Error en on_toggle_vegetariano: {e}")
        finally:
            # Regenerar menús para aplicar el nuevo filtro
            self.generar_menus()

    def on_incluir_ingrediente(self, categoria, nombre):
        """Maneja la selección de un ingrediente para incluir"""
        var_incluir = self.vars_ingredientes[categoria][nombre]["incluir"]
        var_excluir = self.vars_ingredientes[categoria][nombre]["excluir"]
        
        if var_incluir.get():
            # Si se incluye, no puede estar excluido
            var_excluir.set(False)
            self.ingredientes_incluir.add(nombre)
            self.ingredientes_excluir.discard(nombre)
        else:
            self.ingredientes_incluir.discard(nombre)
        
        self.generar_menus()

    def on_excluir_ingrediente(self, categoria, nombre):
        """Maneja la selección de un ingrediente para excluir"""
        var_incluir = self.vars_ingredientes[categoria][nombre]["incluir"]
        var_excluir = self.vars_ingredientes[categoria][nombre]["excluir"]
        
        if var_excluir.get():
            # Si se excluye, no puede estar incluido
            var_incluir.set(False)
            self.ingredientes_excluir.add(nombre)
            self.ingredientes_incluir.discard(nombre)
        else:
            self.ingredientes_excluir.discard(nombre)
        
        self.generar_menus()

    def incluir_todos_categoria(self, categoria):
        """Incluye todos los ingredientes de una categoría"""
        for nombre, vars_dict in self.vars_ingredientes[categoria].items():
            vars_dict["incluir"].set(True)
            vars_dict["excluir"].set(False)
            self.ingredientes_incluir.add(nombre)
            self.ingredientes_excluir.discard(nombre)
        self.generar_menus()

    def excluir_todos_categoria(self, categoria):
        """Excluye todos los ingredientes de una categoría"""
        for nombre, vars_dict in self.vars_ingredientes[categoria].items():
            vars_dict["incluir"].set(False)
            vars_dict["excluir"].set(True)
            self.ingredientes_excluir.add(nombre)
            self.ingredientes_incluir.discard(nombre)
        self.generar_menus()

    def limpiar_categoria(self, categoria):
        """Limpia la selección de todos los ingredientes de una categoría"""
        for nombre, vars_dict in self.vars_ingredientes[categoria].items():
            vars_dict["incluir"].set(False)
            vars_dict["excluir"].set(False)
            self.ingredientes_incluir.discard(nombre)
            self.ingredientes_excluir.discard(nombre)
        self.generar_menus()

    def generar_menus(self):
        """Genera menús basados en los filtros actuales usando Prolog"""
        # Limpiar frame de menús
        for widget in self.frame_menus_scrollable.winfo_children():
            widget.destroy()

        self.tarjetas_widgets = []

        # Preparar parámetros para Prolog
        vegetariano_filter = "true" if self.var_vegetariano.get() else "false"
        con_postre = "true" if self.var_con_postre.get() else "false"
        tipo_carne = self.var_tipo_carne.get().lower()

        # Configurar calorías (usar 'none' si no está especificado)
        try:
            cal_min = int(self.var_cal_min.get()) if self.var_cal_min.get() else None
        except ValueError:
            cal_min = None

        try:
            cal_max = int(self.var_cal_max.get()) if self.var_cal_max.get() else None
        except ValueError:
            cal_max = None

        min_cal_str = str(cal_min) if cal_min is not None else "none"
        max_cal_str = str(cal_max) if cal_max is not None else "none"

        # Preparar listas de ingredientes para Prolog
        incluir_lista = list(self.ingredientes_incluir) if self.ingredientes_incluir else []
        excluir_lista = list(self.ingredientes_excluir) if self.ingredientes_excluir else []
        
        # Consultar Prolog usando el nuevo predicado que extrae componentes con ingredientes
        query = f"""get_menu_details_with_ingredients({vegetariano_filter}, {tipo_carne}, {con_postre}, {min_cal_str}, {max_cal_str}, {incluir_lista}, {excluir_lista},
                    EntradaNom, EntradaCal, EntradaVeg,
                    CarbNom, CarbCal, CarbVeg,
                    CarneNom, CarneCal, CarneTipo, CarneVeg,
                    VegNom, VegCal, VegVeg,
                    PostreNom, PostreCal, PostreVeg,
                    TotalCal)"""

        try:
            # Obtener hasta 10 menús
            results = []
            for i, result in enumerate(self.prolog.query(query)):
                if i >= 10:  # Limitar a 10 menús
                    break
                results.append(result)

            if not results or len(results) == 0:
                ttk.Label(self.frame_menus_scrollable,
                         text="⚠️ No hay menús que cumplan con los filtros seleccionados",
                         foreground="red").pack(pady=20)
                return

            # Convertir menús de Prolog a formato Python
            self.menus_actuales = []
            seen_menus = set()  # Para evitar duplicados

            for result in results:
                # Crear representación de menú para detectar duplicados
                menu_key = (result['EntradaNom'], result['CarbNom'],
                           result['CarneNom'], result['VegNom'], result['PostreNom'])

                # Evitar duplicados
                if menu_key in seen_menus:
                    continue
                seen_menus.add(menu_key)

                menu_dict = self.convertir_resultado_prolog(result)
                self.menus_actuales.append(menu_dict)

                # Limitar a 5 menús únicos
                if len(self.menus_actuales) >= 5:
                    break

            if not self.menus_actuales:
                ttk.Label(self.frame_menus_scrollable,
                         text="⚠️ No hay menús que cumplan con los criterios",
                         foreground="orange").pack(pady=20)
                return

            # Mostrar menús
            self.tarjetas_widgets = []
            for idx, menu in enumerate(self.menus_actuales, 1):
                tarjeta = self.crear_tarjeta_menu(menu, idx)
                self.tarjetas_widgets.append(tarjeta)

            # Organizar tarjetas en grid
            self.reorganizar_tarjetas()

        except Exception as e:
            ttk.Label(self.frame_menus_scrollable,
                     text=f"⚠️ Error al consultar Prolog: {str(e)}",
                     foreground="red").pack(pady=20)
            print(f"Error en query Prolog: {e}")

    def convertir_resultado_prolog(self, result):
        """Convierte un resultado de Prolog con campos individuales a formato Python dict"""
        menu_dict = {
            "entrada": {
                "nombre": str(result['EntradaNom']),
                "calorias": int(result['EntradaCal']),
                "vegetariano": str(result['EntradaVeg']) == "true"
            },
            "carbohidrato": {
                "nombre": str(result['CarbNom']),
                "calorias": int(result['CarbCal']),
                "vegetariano": str(result['CarbVeg']) == "true"
            },
            "carne": {
                "nombre": str(result['CarneNom']),
                "calorias": int(result['CarneCal']),
                "tipo": str(result['CarneTipo']),
                "vegetariano": str(result['CarneVeg']) == "true"
            },
            "vegetal": {
                "nombre": str(result['VegNom']),
                "calorias": int(result['VegCal']),
                "vegetariano": str(result['VegVeg']) == "true"
            },
            "postre": None if str(result['PostreNom']) == "none" else {
                "nombre": str(result['PostreNom']),
                "calorias": int(result['PostreCal']),
                "vegetariano": str(result['PostreVeg']) == "true"
            },
            "calorias": int(result['TotalCal'])
        }

        return menu_dict


    def reorganizar_tarjetas(self, event=None):
        """Reorganiza las tarjetas en grid según el ancho disponible"""
        if not hasattr(self, 'tarjetas_widgets'):
            return

        # Obtener ancho disponible
        ancho_disponible = self.canvas.winfo_width()
        if ancho_disponible <= 1:  # Canvas aún no inicializado
            return

        # Calcular número de columnas (ancho mínimo por tarjeta: 350px)
        ancho_tarjeta = 350
        num_columnas = max(1, ancho_disponible // ancho_tarjeta)

        # Reorganizar en grid
        for idx, tarjeta in enumerate(self.tarjetas_widgets):
            fila = idx // num_columnas
            columna = idx % num_columnas
            tarjeta.grid(row=fila, column=columna, padx=5, pady=5, sticky="nsew")

        # Configurar peso de columnas para distribución uniforme
        for col in range(num_columnas):
            self.frame_menus_scrollable.grid_columnconfigure(col, weight=1)

    def crear_tarjeta_menu(self, menu, numero):
        """Crea una tarjeta visual para mostrar un menú"""
        frame_tarjeta = ttk.Frame(self.frame_menus_scrollable, relief="solid", borderwidth=1)
        # No usar pack aquí, se usará grid en reorganizar_tarjetas

        # Encabezado
        frame_header = ttk.Frame(frame_tarjeta)
        frame_header.pack(fill="x", padx=10, pady=5)

        ttk.Label(frame_header, text=f"Menú #{numero}",
                 font=("Arial", 12, "bold")).pack(side="left")

        ttk.Label(frame_header, text=f"{menu['calorias']} calorías",
                 font=("Arial", 10), foreground="green").pack(side="right")

        # Contenido del menú
        frame_contenido = ttk.Frame(frame_tarjeta)
        frame_contenido.pack(fill="x", padx=20, pady=5)

        items = [
            ("Entrada:", menu["entrada"]["nombre"]),
            ("Carbohidrato:", menu["carbohidrato"]["nombre"]),
            ("Proteína:", menu["carne"]["nombre"]),
            ("Vegetal:", menu["vegetal"]["nombre"]),
        ]

        if menu["postre"]:
            items.append(("Postre:", menu["postre"]["nombre"]))

        for etiqueta, nombre in items:
            frame_item = ttk.Frame(frame_contenido)
            frame_item.pack(fill="x", pady=2)
            ttk.Label(frame_item, text=etiqueta, width=15).pack(side="left")
            ttk.Label(frame_item, text=nombre).pack(side="left")

        # Botones de acción
        frame_botones = ttk.Frame(frame_tarjeta)
        frame_botones.pack(fill="x", padx=10, pady=10)

        ttk.Button(frame_botones, text="✓ Aceptar",
                  command=lambda: self.aceptar_menu(menu)).pack(side="left", padx=5)

        ttk.Button(frame_botones, text="✗ Rechazar",
                  command=lambda: self.rechazar_menu(menu)).pack(side="left", padx=5)

        return frame_tarjeta

    def aceptar_menu(self, menu):
        """Registra la aceptación de un menú"""
        registro = {
            "menu": menu,
            "accion": "aceptado",
            "timestamp": datetime.now(),
            "filtros": self.obtener_estado_filtros()
        }
        self.historial.append(registro)
        self.actualizar_estadisticas()
        messagebox.showinfo("Éxito", "¡Menú aceptado! 👍")
        # Aquí en el futuro se actualizará el aprendizaje en Prolog

    def rechazar_menu(self, menu):
        """Registra el rechazo de un menú"""
        registro = {
            "menu": menu,
            "accion": "rechazado",
            "timestamp": datetime.now(),
            "filtros": self.obtener_estado_filtros()
        }
        self.historial.append(registro)
        self.actualizar_estadisticas()
        messagebox.showinfo("Registrado", "Menú rechazado. Se aprenderá de tu preferencia. 👎")

    def obtener_estado_filtros(self):
        """Obtiene el estado actual de todos los filtros"""
        return {
            "vegetariano": self.var_vegetariano.get(),
            "con_postre": self.var_con_postre.get(),
            "tipo_carne": self.var_tipo_carne.get(),
            "cal_min": self.var_cal_min.get(),
            "cal_max": self.var_cal_max.get()
        }

    def actualizar_estadisticas(self):
        """Actualiza las estadísticas mostradas"""
        aceptados = sum(1 for h in self.historial if h["accion"] == "aceptado")
        rechazados = sum(1 for h in self.historial if h["accion"] == "rechazado")
        total = len(self.historial)

        self.label_stats.config(
            text=f"Aceptados: {aceptados} | Rechazados: {rechazados} | Total: {total}"
        )

# ============================================================================
# PUNTO DE ENTRADA
# ============================================================================

if __name__ == "__main__":
    root = tk.Tk()
    app = MenuSaludableApp(root)
    root.mainloop()
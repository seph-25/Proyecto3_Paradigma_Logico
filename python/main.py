#!/usr/bin/env python3
import tkinter as tk
from tkinter import ttk, messagebox
from itertools import product
import random
from datetime import datetime

# ============================================================================
# BASE DE DATOS DE ALIMENTOS
# ============================================================================

ENTRADAS = [
    {"nombre": "Ensalada César", "calorias": 150, "vegetariano": False},
    {"nombre": "Ensalada Verde", "calorias": 80, "vegetariano": True},
    {"nombre": "Sopa de Verduras", "calorias": 120, "vegetariano": True},
    {"nombre": "Sopa de Pollo", "calorias": 180, "vegetariano": False},
    {"nombre": "Carpaccio de Res", "calorias": 200, "vegetariano": False},
    {"nombre": "Bruschetta", "calorias": 140, "vegetariano": True},
    {"nombre": "Gazpacho", "calorias": 100, "vegetariano": True},
    {"nombre": "Ceviche", "calorias": 160, "vegetariano": False},
]

CARBOHIDRATOS = [
    {"nombre": "Arroz Integral", "calorias": 215, "vegetariano": True},
    {"nombre": "Arroz Blanco", "calorias": 205, "vegetariano": True},
    {"nombre": "Pasta Integral", "calorias": 180, "vegetariano": True},
    {"nombre": "Quinoa", "calorias": 120, "vegetariano": True},
    {"nombre": "Puré de Papa", "calorias": 170, "vegetariano": True},
    {"nombre": "Batata Asada", "calorias": 160, "vegetariano": True},
    {"nombre": "Pan Integral", "calorias": 140, "vegetariano": True},
]

CARNES = [
    {"nombre": "Pollo a la Plancha", "calorias": 165, "tipo": "pollo", "vegetariano": False},
    {"nombre": "Pollo al Horno", "calorias": 190, "tipo": "pollo", "vegetariano": False},
    {"nombre": "Res a la Plancha", "calorias": 250, "tipo": "res", "vegetariano": False},
    {"nombre": "Lomo de Res", "calorias": 280, "tipo": "res", "vegetariano": False},
    {"nombre": "Pescado al Vapor", "calorias": 150, "tipo": "pescado", "vegetariano": False},
    {"nombre": "Salmón a la Plancha", "calorias": 206, "tipo": "pescado", "vegetariano": False},
    {"nombre": "Atún Sellado", "calorias": 184, "tipo": "pescado", "vegetariano": False},
    {"nombre": "Cerdo Agridulce", "calorias": 230, "tipo": "cerdo", "vegetariano": False},
    {"nombre": "Tofu Salteado", "calorias": 145, "tipo": "vegetariano", "vegetariano": True},
    {"nombre": "Lentejas Guisadas", "calorias": 230, "tipo": "vegetariano", "vegetariano": True},
    {"nombre": "Garbanzos al Curry", "calorias": 210, "tipo": "vegetariano", "vegetariano": True},
]

VEGETALES = [
    {"nombre": "Brócoli al Vapor", "calorias": 55, "vegetariano": True},
    {"nombre": "Espinacas Salteadas", "calorias": 40, "vegetariano": True},
    {"nombre": "Zanahorias Glaseadas", "calorias": 80, "vegetariano": True},
    {"nombre": "Judías Verdes", "calorias": 44, "vegetariano": True},
    {"nombre": "Calabacín a la Plancha", "calorias": 33, "vegetariano": True},
    {"nombre": "Champiñones Salteados", "calorias": 28, "vegetariano": True},
    {"nombre": "Espárragos", "calorias": 27, "vegetariano": True},
    {"nombre": "Col Rizada", "calorias": 49, "vegetariano": True},
]

POSTRES = [
    {"nombre": "Fruta Fresca", "calorias": 60, "vegetariano": True},
    {"nombre": "Yogur Natural", "calorias": 100, "vegetariano": True},
    {"nombre": "Gelatina", "calorias": 80, "vegetariano": True},
    {"nombre": "Mousse de Chocolate", "calorias": 150, "vegetariano": True},
    {"nombre": "Flan", "calorias": 140, "vegetariano": True},
    {"nombre": "Helado de Vainilla", "calorias": 137, "vegetariano": True},
    {"nombre": "Tarta de Manzana", "calorias": 180, "vegetariano": True},
]

# ============================================================================
# CLASE PRINCIPAL
# ============================================================================

class MenuSaludableApp:
    def __init__(self, root):
        self.root = root
        self.root.title("Menú Saludable Inteligente")
        self.root.geometry("900x700")

        # Historial de aceptaciones/rechazos
        self.historial = []

        # Variables de filtros
        self.var_vegetariano = tk.BooleanVar(value=False)
        self.var_con_postre = tk.BooleanVar(value=True)
        self.var_tipo_carne = tk.StringVar(value="Todas")
        self.var_cal_min = tk.StringVar(value="")
        self.var_cal_max = tk.StringVar(value="")

        # Menús generados
        self.menus_actuales = []

        self.create_widgets()

    def create_widgets(self):
        # ===== FRAME DE FILTROS =====
        frame_filtros = ttk.LabelFrame(self.root, text="Filtros", padding=10)
        frame_filtros.pack(fill="x", padx=10, pady=5)

        # Fila 1: Vegetariano y Postre
        fila1 = ttk.Frame(frame_filtros)
        fila1.pack(fill="x", pady=5)

        ttk.Checkbutton(fila1, text="Solo Vegetariano",
                       variable=self.var_vegetariano,
                       command=self.generar_menus).pack(side="left", padx=10)

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

        # ===== FRAME DE ESTADÍSTICAS =====
        frame_stats = ttk.LabelFrame(self.root, text="Estadísticas", padding=10)
        frame_stats.pack(fill="x", padx=10, pady=5)

        self.label_stats = ttk.Label(frame_stats, text="Aceptados: 0 | Rechazados: 0 | Total: 0")
        self.label_stats.pack()

        # Generar menús iniciales
        self.generar_menus()

    def generar_menus(self):
        """Genera menús basados en los filtros actuales"""
        # Limpiar frame de menús
        for widget in self.frame_menus_scrollable.winfo_children():
            widget.destroy()

        self.tarjetas_widgets = []

        # Aplicar filtros
        entradas_filtradas = self.filtrar_alimentos(ENTRADAS)
        carbohidratos_filtrados = self.filtrar_alimentos(CARBOHIDRATOS)
        carnes_filtradas = self.filtrar_carnes(CARNES)
        vegetales_filtrados = self.filtrar_alimentos(VEGETALES)
        postres_filtrados = self.filtrar_alimentos(POSTRES) if self.var_con_postre.get() else [None]

        # Validar que hay opciones
        if not (entradas_filtradas and carbohidratos_filtrados and
                carnes_filtradas and vegetales_filtrados):
            ttk.Label(self.frame_menus_scrollable,
                     text="⚠️ No hay suficientes opciones con los filtros seleccionados",
                     foreground="red").pack(pady=20)
            return

        # Generar todas las combinaciones posibles
        todas_combinaciones = list(product(
            entradas_filtradas,
            carbohidratos_filtrados,
            carnes_filtradas,
            vegetales_filtrados,
            postres_filtrados
        ))

        # Filtrar por calorías
        combinaciones_validas = []
        for combo in todas_combinaciones:
            entrada, carb, carne, vegetal, postre = combo
            calorias_totales = (entrada["calorias"] + carb["calorias"] +
                              carne["calorias"] + vegetal["calorias"])
            if postre:
                calorias_totales += postre["calorias"]

            # Verificar rango de calorías
            if not self.cumple_calorias(calorias_totales):
                continue

            combinaciones_validas.append({
                "entrada": entrada,
                "carbohidrato": carb,
                "carne": carne,
                "vegetal": vegetal,
                "postre": postre,
                "calorias": calorias_totales
            })

        if not combinaciones_validas:
            ttk.Label(self.frame_menus_scrollable,
                     text="⚠️ No hay menús que cumplan con los criterios de calorías",
                     foreground="orange").pack(pady=20)
            return

        # Seleccionar menús diversos (máximo 5)
        self.menus_actuales = self.seleccionar_menus_diversos(combinaciones_validas, max_menus=5)

        # Mostrar menús
        self.tarjetas_widgets = []
        for idx, menu in enumerate(self.menus_actuales, 1):
            tarjeta = self.crear_tarjeta_menu(menu, idx)
            self.tarjetas_widgets.append(tarjeta)

        # Organizar tarjetas en grid
        self.reorganizar_tarjetas()

    def filtrar_alimentos(self, lista_alimentos):
        """Filtra alimentos según si es vegetariano"""
        if self.var_vegetariano.get():
            return [a for a in lista_alimentos if a["vegetariano"]]
        return lista_alimentos

    def filtrar_carnes(self, lista_carnes):
        """Filtra carnes según tipo seleccionado"""
        tipo = self.var_tipo_carne.get().lower()

        if self.var_vegetariano.get():
            return [c for c in lista_carnes if c["vegetariano"]]

        if tipo == "todas":
            return lista_carnes

        return [c for c in lista_carnes if c.get("tipo", "").lower() == tipo]

    def cumple_calorias(self, calorias_totales):
        """Verifica si las calorías están en el rango especificado"""
        try:
            cal_min = int(self.var_cal_min.get()) if self.var_cal_min.get() else None
            cal_max = int(self.var_cal_max.get()) if self.var_cal_max.get() else None

            if cal_min and calorias_totales < cal_min:
                return False
            if cal_max and calorias_totales > cal_max:
                return False
            return True
        except ValueError:
            return True

    def seleccionar_menus_diversos(self, combinaciones, max_menus=5):
        """Selecciona menús que sean suficientemente diferentes entre sí"""
        if len(combinaciones) <= max_menus:
            return combinaciones

        # Estrategia: seleccionar menús con diferentes combinaciones de ingredientes
        seleccionados = []
        random.shuffle(combinaciones)

        for combo in combinaciones:
            if len(seleccionados) >= max_menus:
                break

            # Verificar que sea suficientemente diferente
            if self.es_suficientemente_diferente(combo, seleccionados):
                seleccionados.append(combo)

        # Si no se llenó, agregar los restantes
        while len(seleccionados) < max_menus and len(seleccionados) < len(combinaciones):
            for combo in combinaciones:
                if combo not in seleccionados:
                    seleccionados.append(combo)
                    break

        return seleccionados

    def es_suficientemente_diferente(self, menu, lista_menus):
        """Verifica que un menú sea diferente a los ya seleccionados"""
        if not lista_menus:
            return True

        for menu_existente in lista_menus:
            diferencias = 0
            if menu["entrada"]["nombre"] != menu_existente["entrada"]["nombre"]:
                diferencias += 1
            if menu["carbohidrato"]["nombre"] != menu_existente["carbohidrato"]["nombre"]:
                diferencias += 1
            if menu["carne"]["nombre"] != menu_existente["carne"]["nombre"]:
                diferencias += 1
            if menu["vegetal"]["nombre"] != menu_existente["vegetal"]["nombre"]:
                diferencias += 1

            # Si tiene menos de 2 diferencias, es muy similar
            if diferencias < 2:
                return False

        return True

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
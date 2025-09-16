#!/usr/bin/env python3
"""
RED NEURONAL BÁSICA PARA PREDICCIÓN DE PRECIOS - VERSIÓN SIMPLIFICADA
Sin try/catch ni validaciones - Código directo y didáctico

🎯 OBJETIVO: Predecir el precio de mañana usando los precios de los últimos días
📚 CONCEPTOS: Neuronas, capas, entrenamiento, predicción
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.preprocessing import MinMaxScaler
from sklearn.model_selection import train_test_split
import tensorflow as tf
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense
import yaml
import warnings
warnings.filterwarnings('ignore')

print("🧠 RED NEURONAL BÁSICA - TUTORIAL PARA PRINCIPIANTES")

# ============================================================================
# PASO 1: CARGAR Y PREPARAR LOS DATOS
# ============================================================================

def cargar_datos_simples(stock_symbol):
    """
    📊 PASO 1: Cargar datos de una acción
    
    ¿Qué hace?
    - Lee el archivo CSV con precios históricos
    - Prepara los datos para la red neuronal
    """
    print(f"\n📈 Cargando datos para {stock_symbol}...")
    
    # Leer el archivo CSV
    archivo = f"data/processed/{stock_symbol}_full.csv"
    datos = pd.read_csv(archivo)
    
    # Solo necesitamos los precios de cierre
    precios = datos['close'].values
    
    print(f"✅ Datos cargados: {len(precios)} días de precios")
    print(f"💰 Precio más bajo: ${min(precios):.2f}")
    print(f"💰 Precio más alto: ${max(precios):.2f}")
    
    return precios

# ============================================================================
# PASO 2: CREAR DATOS PARA ENTRENAR
# ============================================================================

def crear_datos_entrenamiento(precios, ventana=5):
    """
    🔧 PASO 2: Preparar datos para la red neuronal
    
    ¿Qué hace?
    - Toma los precios de los últimos N días (ventana)
    - Los usa para predecir el precio del día siguiente
    
    Ejemplo:
    Si ventana=5:
    - Entrada: precios días [1,2,3,4,5] → Salida: precio día 6
    - Entrada: precios días [2,3,4,5,6] → Salida: precio día 7
    """
    print(f"\n🔧 Creando datos de entrenamiento con ventana de {ventana} días...")
    
    X = []  # Entradas (precios de los últimos N días)
    y = []  # Salidas (precio del día siguiente)
    
    # Recorrer todos los precios
    for i in range(ventana, len(precios)):
        entrada = precios[i-ventana:i]  # Últimos N días
        salida = precios[i]             # Precio siguiente
        
        X.append(entrada)
        y.append(salida)
    
    # Convertir a arrays de numpy
    X = np.array(X)
    y = np.array(y)
    
    print(f"✅ Datos creados:")
    print(f"   📥 Entradas (X): {X.shape} - cada fila son {ventana} días")
    print(f"   📤 Salidas (y): {y.shape} - cada valor es el precio siguiente")
    
    return X, y

# ============================================================================
# PASO 3: NORMALIZAR LOS DATOS
# ============================================================================

def normalizar_datos(X, y):
    """
    📏 PASO 3: Normalizar datos (escalar entre 0 y 1)
    
    ¿Por qué normalizar?
    - Las redes neuronales funcionan mejor con números pequeños
    - Si los precios están en miles, la red se "confunde"
    - Normalizamos para que todos los valores estén entre 0 y 1
    """
    print(f"\n📏 Normalizando datos...")
    
    # Crear normalizadores
    scaler_X = MinMaxScaler()
    scaler_y = MinMaxScaler()
    
    # Normalizar entradas
    X_norm = np.zeros_like(X)
    for i in range(X.shape[0]):
        X_norm[i] = scaler_X.fit_transform(X[i].reshape(-1, 1)).flatten()
    
    # Normalizar salidas
    y_norm = scaler_y.fit_transform(y.reshape(-1, 1)).flatten()
    
    print(f"✅ Datos normalizados:")
    print(f"   📥 X: de [{X.min():.2f}, {X.max():.2f}] → [{X_norm.min():.3f}, {X_norm.max():.3f}]")
    print(f"   📤 y: de [{y.min():.2f}, {y.max():.2f}] → [{y_norm.min():.3f}, {y_norm.max():.3f}]")
    
    return X_norm, y_norm, scaler_X, scaler_y

# ============================================================================
# PASO 4: CONSTRUIR LA RED NEURONAL
# ============================================================================

def crear_red_neuronal_simple(num_entradas):
    """
    🧠 PASO 4: Construir la red neuronal
    
    ¿Qué es una red neuronal?
    - Como un cerebro simple con neuronas conectadas
    - Cada neurona recibe información, la procesa, y pasa el resultado
    
    Nuestra red tendrá:
    - Capa 1: 50 neuronas (reciben los precios históricos)
    - Capa 2: 25 neuronas (procesan la información)
    - Capa 3: 1 neurona (da la predicción final)
    """
    print(f"\n🧠 Construyendo red neuronal...")
    
    # Crear la red neuronal secuencial
    modelo = Sequential()
    
    # CAPA 1: 60 neuronas
    modelo.add(Dense(60, input_dim=num_entradas, activation='relu'))
    print("   🔗 Capa 1: 50 neuronas (procesamiento inicial)")
    
    # CAPA 2: 25 neuronas
    modelo.add(Dense(25, activation='relu'))
    print("   🔗 Capa 2: 25 neuronas (procesamiento intermedio)")
    
    # CAPA 3: 1 neurona (predicción final)
    modelo.add(Dense(1, activation='linear'))
    print("   🔗 Capa 3: 1 neurona (predicción final)")
    
    # COMPILAR la red
    modelo.compile(optimizer='adam', loss='mse', metrics=['mae'])
    print("   ⚙️ Red compilada con optimizador Adam")
    
    # Mostrar resumen
    print(f"\n📋 RESUMEN DE LA RED NEURONAL:")
    modelo.summary()
    
    return modelo

# ============================================================================
# PASO 5: ENTRENAR LA RED NEURONAL
# ============================================================================

def entrenar_red(modelo, X_train, y_train, X_val, y_val):
    """
    🎓 PASO 5: Entrenar la red neuronal
    
    ¿Cómo aprende la red?
    1. Ve ejemplos (precios históricos → precio siguiente)
    2. Hace una predicción
    3. Compara con la respuesta correcta
    4. Ajusta sus "neuronas" para mejorar
    5. Repite miles de veces (épocas)
    """
    print(f"\n🎓 Entrenando la red neuronal...")
    
    # PARÁMETROS DE ENTRENAMIENTO
    epochs = 350      # Cuántas veces ver todos los datos
    batch_size = 32   # Cuántos ejemplos procesar a la vez
    
    print(f"   📚 Épocas: {epochs}")
    print(f"   📦 Batch size: {batch_size}")
    print(f"   ⏱️ Iniciando entrenamiento...")
    
    # ENTRENAR
    history = modelo.fit(
        X_train, y_train,
        epochs=epochs,
        batch_size=batch_size,
        validation_data=(X_val, y_val),
        verbose=1
    )
    
    print(f"✅ Entrenamiento completado!")
    return history

# ============================================================================
# PASO 6: HACER PREDICCIONES
# ============================================================================

def hacer_predicciones(modelo, X_test, scaler_y):
    """
    🔮 PASO 6: Hacer predicciones con la red entrenada
    
    ¿Cómo predice?
    1. Le damos precios de los últimos días
    2. La red procesa la información en sus capas
    3. Nos da una predicción normalizada
    4. La convertimos de vuelta al precio real
    """
    print(f"\n🔮 Haciendo predicciones...")
    
    # Predicciones normalizadas (entre 0 y 1)
    predicciones_norm = modelo.predict(X_test)
    
    # Convertir de vuelta a precios reales
    predicciones_reales = scaler_y.inverse_transform(predicciones_norm)
    
    print(f"✅ Predicciones realizadas: {len(predicciones_reales)} valores")
    
    return predicciones_reales.flatten()

# ============================================================================
# PASO 7: EVALUAR LA RED
# ============================================================================

def evaluar_modelo(y_real, y_pred):
    """
    📊 PASO 7: Ver qué tan buena es la red
    
    Métricas importantes:
    - MAE (Error Absoluto Medio): promedio de errores en dólares
    - RMSE (Error Cuadrático Medio): penaliza errores grandes
    - Precisión direccional: % de veces que acierta si sube/baja
    """
    print(f"\n📊 Evaluando el modelo...")
    
    # Error Absoluto Medio
    mae = np.mean(np.abs(y_real - y_pred))
    
    # Error Cuadrático Medio
    rmse = np.sqrt(np.mean((y_real - y_pred)**2))
    
    # Precisión direccional (¿acierta si sube o baja?)
    direccion_real = np.diff(y_real) > 0
    direccion_pred = np.diff(y_pred) > 0
    precision_direccional = np.mean(direccion_real == direccion_pred) * 100
    
    print(f"📈 MÉTRICAS DE EVALUACIÓN:")
    print(f"   💲 MAE (Error promedio): ${mae:.2f}")
    print(f"   📏 RMSE (Error cuadrático): ${rmse:.2f}")
    print(f"   🎯 Precisión direccional: {precision_direccional:.1f}%")
    
    return mae, rmse, precision_direccional

# ============================================================================
# PASO 8: VISUALIZAR RESULTADOS
# ============================================================================

def graficar_resultados(y_real, y_pred, stock_symbol, titulo="Predicciones vs Realidad"):
    """
    📈 PASO 8: Crear gráficos para ver los resultados
    """
    print(f"\n📈 Creando gráficos...")
    
    fig, axes = plt.subplots(2, 2, figsize=(15, 10))
    fig.suptitle(titulo, fontsize=16, fontweight='bold')
    
    # Gráfico 1: Predicciones vs Valores Reales
    axes[0,0].plot(y_real, label='Precios Reales', color='blue', linewidth=2)
    axes[0,0].plot(y_pred, label='Predicciones', color='red', linewidth=2, alpha=0.7)
    axes[0,0].set_title('Predicciones vs Realidad')
    axes[0,0].set_xlabel('Días')
    axes[0,0].set_ylabel('Precio ($)')
    axes[0,0].legend()
    axes[0,0].grid(True, alpha=0.3)
    
    # Gráfico 2: Scatter plot (correlación)
    axes[0,1].scatter(y_real, y_pred, alpha=0.6, color='green')
    axes[0,1].plot([y_real.min(), y_real.max()], [y_real.min(), y_real.max()], 'r--')
    axes[0,1].set_title('Correlación: Real vs Predicho')
    axes[0,1].set_xlabel('Precio Real ($)')
    axes[0,1].set_ylabel('Precio Predicho ($)')
    axes[0,1].grid(True, alpha=0.3)
    
    # Gráfico 3: Errores
    errores = y_real - y_pred
    axes[1,0].plot(errores, color='orange', linewidth=1)
    axes[1,0].axhline(y=0, color='red', linestyle='--')
    axes[1,0].set_title('Errores de Predicción')
    axes[1,0].set_xlabel('Días')
    axes[1,0].set_ylabel('Error ($)')
    axes[1,0].grid(True, alpha=0.3)
    
    # Gráfico 4: Distribución de errores
    axes[1,1].hist(errores, bins=30, color='purple', alpha=0.7)
    axes[1,1].axvline(x=0, color='red', linestyle='--')
    axes[1,1].set_title('Distribución de Errores')
    axes[1,1].set_xlabel('Error ($)')
    axes[1,1].set_ylabel('Frecuencia')
    axes[1,1].grid(True, alpha=0.3)
    
    plt.tight_layout()
    plt.savefig(f"output/plots/{stock_symbol}_neural_network.png")
    plt.show()
    
    print(f"✅ Gráficos creados")

# ============================================================================
# FUNCIÓN PRINCIPAL - EJECUTA TODO EL PROCESO
# ============================================================================

def red_neuronal_basica(stock_symbol, ventana_dias=3):
    """
    🚀 FUNCIÓN PRINCIPAL: Ejecuta todo el proceso paso a paso
    """
    print(f"\n🚀 INICIANDO RED NEURONAL BÁSICA PARA {stock_symbol}")
    
    # PASO 1: Cargar datos
    precios = cargar_datos_simples(stock_symbol)
    
    # PASO 2: Crear datos de entrenamiento
    X, y = crear_datos_entrenamiento(precios, ventana_dias)
    
    # PASO 3: Normalizar
    X_norm, y_norm, scaler_X, scaler_y = normalizar_datos(X, y)
    
    # Dividir en entrenamiento y prueba (70% - 30%)
    X_train, X_test, y_train, y_test = train_test_split(
        X_norm, y_norm, test_size=0.3, shuffle=False
    )
    
    print(f"\n📊 División de datos:")
    print(f"   🎓 Entrenamiento: {len(X_train)} ejemplos")
    print(f"   🧪 Prueba: {len(X_test)} ejemplos")
    
    # PASO 4: Construir red neuronal
    modelo = crear_red_neuronal_simple(ventana_dias)
    
    # PASO 5: Entrenar
    history = entrenar_red(modelo, X_train, y_train, X_test, y_test)
    
    # PASO 6: Hacer predicciones
    predicciones = hacer_predicciones(modelo, X_test, scaler_y)
    
    # Convertir y_test de vuelta a precios reales
    y_test_real = scaler_y.inverse_transform(y_test.reshape(-1, 1)).flatten()
    
    # PASO 7: Evaluar
    mae, rmse, precision = evaluar_modelo(y_test_real, predicciones)
    
    # PASO 8: Visualizar
    graficar_resultados(y_test_real, predicciones, stock_symbol, 
                       f'Red Neuronal Básica - {stock_symbol}')
    
    # Guardar modelo
    modelo.save(f'models/NN/modelo_nn_{stock_symbol}.keras')
    print(f"💾 Modelo guardado como: modelo_nn_{stock_symbol}.keras")
    
    print(f"\n🎉 ¡PROCESO COMPLETADO EXITOSAMENTE!")
    print(f"📈 La red neuronal puede predecir precios con un error promedio de ${mae:.2f}")
    
    return modelo, predicciones, mae, rmse, precision

# ============================================================================
# EJECUCIÓN PRINCIPAL
# ============================================================================

if __name__ == "__main__":
    # Cargar configuración
    with open("params.yaml", 'r') as file:
        params = yaml.safe_load(file)
    
    # Usar solo ABEV como ejemplo
    stocks = params['stocks']
    print(f"📋 Acciones a procesar: {stocks}")
    
    # Ejecutar para cada acción
    resultados = {}
    
    for stock in stocks:
        # Ejecutar red neuronal básica
        modelo, pred, mae, rmse, precision = red_neuronal_basica(stock, ventana_dias=7)
        
        resultados[stock] = {
            'modelo': modelo,
            'predicciones': pred,
            'mae': mae,
            'rmse': rmse,
            'precision_direccional': precision
        }
        
        print(f"✅ {stock} completado - MAE: ${mae:.2f}")
    
    # Resumen final
    print(f"\n" + "🎊"*20)
    print(f"RESUMEN FINAL - RED NEURONAL BÁSICA")
    print(f"🎊"*20)
    
    for stock, resultado in resultados.items():
        mae = resultado['mae']
        precision = resultado['precision_direccional']
        print(f"📊 {stock}: Error ${mae:.2f} - Precisión direccional {precision:.1f}%")
    
    print(f"\n💡 CONCEPTOS APRENDIDOS:")
    print(f"   🧠 Qué es una red neuronal y cómo funciona")
    print(f"   📊 Cómo preparar datos para machine learning")
    print(f"   📏 Importancia de normalizar los datos")
    print(f"   🎓 Proceso de entrenamiento")
    print(f"   🔮 Cómo hacer predicciones")
    print(f"   📈 Cómo evaluar un modelo")
    
    print(f"\n🚀 ¡FELICITACIONES! Ya sabes lo básico de redes neuronales")

USE [master]
GO
/****** Object:  Database [SQLDB_CONCESIONARIA]    Script Date: 2/2/2024 12:31:11 AM ******/
CREATE DATABASE [SQLDB_CONCESIONARIA]
 CONTAINMENT = NONE
 ON  PRIMARY 
( NAME = N'SQLDB_CONCESIONARIA', FILENAME = N'C:\Users\denni\SQLDB_CONCESIONARIA.mdf' , SIZE = 8192KB , MAXSIZE = UNLIMITED, FILEGROWTH = 65536KB )
 LOG ON 
( NAME = N'SQLDB_CONCESIONARIA_log', FILENAME = N'C:\Users\denni\SQLDB_CONCESIONARIA_log.ldf' , SIZE = 8192KB , MAXSIZE = 2048GB , FILEGROWTH = 65536KB )
 WITH CATALOG_COLLATION = DATABASE_DEFAULT
GO
ALTER DATABASE [SQLDB_CONCESIONARIA] SET COMPATIBILITY_LEVEL = 150
GO
IF (1 = FULLTEXTSERVICEPROPERTY('IsFullTextInstalled'))
begin
EXEC [SQLDB_CONCESIONARIA].[dbo].[sp_fulltext_database] @action = 'enable'
end
GO
ALTER DATABASE [SQLDB_CONCESIONARIA] SET ANSI_NULL_DEFAULT OFF 
GO
ALTER DATABASE [SQLDB_CONCESIONARIA] SET ANSI_NULLS OFF 
GO
ALTER DATABASE [SQLDB_CONCESIONARIA] SET ANSI_PADDING OFF 
GO
ALTER DATABASE [SQLDB_CONCESIONARIA] SET ANSI_WARNINGS OFF 
GO
ALTER DATABASE [SQLDB_CONCESIONARIA] SET ARITHABORT OFF 
GO
ALTER DATABASE [SQLDB_CONCESIONARIA] SET AUTO_CLOSE ON 
GO
ALTER DATABASE [SQLDB_CONCESIONARIA] SET AUTO_SHRINK OFF 
GO
ALTER DATABASE [SQLDB_CONCESIONARIA] SET AUTO_UPDATE_STATISTICS ON 
GO
ALTER DATABASE [SQLDB_CONCESIONARIA] SET CURSOR_CLOSE_ON_COMMIT OFF 
GO
ALTER DATABASE [SQLDB_CONCESIONARIA] SET CURSOR_DEFAULT  GLOBAL 
GO
ALTER DATABASE [SQLDB_CONCESIONARIA] SET CONCAT_NULL_YIELDS_NULL OFF 
GO
ALTER DATABASE [SQLDB_CONCESIONARIA] SET NUMERIC_ROUNDABORT OFF 
GO
ALTER DATABASE [SQLDB_CONCESIONARIA] SET QUOTED_IDENTIFIER OFF 
GO
ALTER DATABASE [SQLDB_CONCESIONARIA] SET RECURSIVE_TRIGGERS OFF 
GO
ALTER DATABASE [SQLDB_CONCESIONARIA] SET  ENABLE_BROKER 
GO
ALTER DATABASE [SQLDB_CONCESIONARIA] SET AUTO_UPDATE_STATISTICS_ASYNC OFF 
GO
ALTER DATABASE [SQLDB_CONCESIONARIA] SET DATE_CORRELATION_OPTIMIZATION OFF 
GO
ALTER DATABASE [SQLDB_CONCESIONARIA] SET TRUSTWORTHY OFF 
GO
ALTER DATABASE [SQLDB_CONCESIONARIA] SET ALLOW_SNAPSHOT_ISOLATION OFF 
GO
ALTER DATABASE [SQLDB_CONCESIONARIA] SET PARAMETERIZATION SIMPLE 
GO
ALTER DATABASE [SQLDB_CONCESIONARIA] SET READ_COMMITTED_SNAPSHOT ON 
GO
ALTER DATABASE [SQLDB_CONCESIONARIA] SET HONOR_BROKER_PRIORITY OFF 
GO
ALTER DATABASE [SQLDB_CONCESIONARIA] SET RECOVERY SIMPLE 
GO
ALTER DATABASE [SQLDB_CONCESIONARIA] SET  MULTI_USER 
GO
ALTER DATABASE [SQLDB_CONCESIONARIA] SET PAGE_VERIFY CHECKSUM  
GO
ALTER DATABASE [SQLDB_CONCESIONARIA] SET DB_CHAINING OFF 
GO
ALTER DATABASE [SQLDB_CONCESIONARIA] SET FILESTREAM( NON_TRANSACTED_ACCESS = OFF ) 
GO
ALTER DATABASE [SQLDB_CONCESIONARIA] SET TARGET_RECOVERY_TIME = 60 SECONDS 
GO
ALTER DATABASE [SQLDB_CONCESIONARIA] SET DELAYED_DURABILITY = DISABLED 
GO
ALTER DATABASE [SQLDB_CONCESIONARIA] SET ACCELERATED_DATABASE_RECOVERY = OFF  
GO
ALTER DATABASE [SQLDB_CONCESIONARIA] SET QUERY_STORE = OFF
GO
USE [SQLDB_CONCESIONARIA]
GO
/****** Object:  UserDefinedTableType [dbo].[tablaVerificar]    Script Date: 2/2/2024 12:31:11 AM ******/
CREATE TYPE [dbo].[tablaVerificar] AS TABLE(
	[Tabla] [nvarchar](max) NOT NULL,
	[NRelaciones] [int] NULL DEFAULT ((0)),
	[Campo] [nvarchar](max) NULL DEFAULT (''),
	[Valor] [nvarchar](max) NULL DEFAULT (''),
	[ResultadoFinal] [int] NULL DEFAULT ((0))
)
GO
/****** Object:  UserDefinedFunction [dbo].[FuncionString]    Script Date: 2/2/2024 12:31:11 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE FUNCTION [dbo].[FuncionString](@NColumnas INT, @Columnas NVARCHAR(MAX), @Columnas2 NVARCHAR(MAX), @Tipo NVARCHAR(50)) 
	RETURNS NVARCHAR(MAX)
AS
BEGIN
	DECLARE @Indice INT = 1
	DECLARE @NColumna1 NVARCHAR(MAX)
	DECLARE @NColumna2 NVARCHAR(MAX)
	DECLARE @caracterEspecial NVARCHAR(MAX);

	DECLARE @CMD NVARCHAR(MAX) = '';
	WHILE @Indice <= @NColumnas BEGIN

		DECLARE @char CHAR = '(';
		SET @caracterEspecial = SUBSTRING(@Columnas2, 0, CHARINDEX(@char, @Columnas2, 0)+1);		
		SET @Columnas2 = TRIM(STUFF(@Columnas2, 1, LEN(@caracterEspecial), ''))
		
		/* Sacamos nuestra cadena  */
		IF @Indice != @NColumnas BEGIN

			SET @NColumna1 = SUBSTRING(@Columnas, 0, CHARINDEX(',', @Columnas, 0))
			SET @NColumna2 = SUBSTRING(@Columnas2, 0, CHARINDEX('),', @Columnas2, 0))
			
			/*  lo reemplzamos en nuestra cadena principal */
			SET @Columnas = REPLACE(@Columnas, @NColumna1+',', '')
			SET @Columnas2 = TRIM(STUFF(@Columnas2, 1, LEN(@NColumna2+'),'), ''))

		END
		ELSE BEGIN
			SET @char = ')';	
			SET @Columnas2 = STUFF(@Columnas2, LEN(SUBSTRING(@Columnas2, 0, CHARINDEX(@char, @Columnas2, 0)))+1, 1, '');

			SET @NColumna1 = TRIM(@Columnas);
			SET @NColumna2 = TRIM(@Columnas2);
		END
		
		IF LOWER(@Tipo) = 'listar' BEGIN 
			SET @CMD += ' INNER JOIN ' + @NColumna1 + ' ON ' + @NColumna2
		END
		
		IF LOWER(@Tipo) = 'crear' BEGIN
			SET @CMD += '''' + TRIM(@NColumna2) + '''';
			IF @Indice != @NColumnas BEGIN
				SET @CMD += ',';
			END
		END

		IF LOWER(@Tipo) = 'actualizar' BEGIN 
			SET @CMD += @NColumna1 + ' = ';
			SET @CMD += '''' + TRIM(@NColumna2) + '''';

			IF @Indice != @NColumnas BEGIN 
				SET @CMD += ', ';
			END
		END

		IF LOWER(@Tipo) = 'actualizar2' BEGIN 
			SET @CMD += @NColumna1 + ' = ';
			SET @CMD += '''' + TRIM(@NColumna2) + '''';

			IF @Indice != @NColumnas BEGIN 
				SET @CMD += ' AND ';
			END
		END

		IF LOWER(@Tipo) = 'verificar' BEGIN 
			SET @CMD += @NColumna1 + ' = ' + ''''+ @NColumna2 + '''';

			IF @Indice != @NColumnas BEGIN 
				SET @CMD += ' AND ';
			END
		END
		
		SET @Indice += 1
	END
	
	RETURN @CMD;
END
GO
/****** Object:  UserDefinedFunction [dbo].[Verificar]    Script Date: 2/2/2024 12:31:11 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE FUNCTION [dbo].[Verificar]( @informacion tablaVerificar READONLY, @estado INT = 0 , @numeroLlaves INT, @minimo INT = 0, @maximo INT = 0)
	RETURNS @verificado TABLE( resultado NVARCHAR(MAX) )
AS
BEGIN 	
	DECLARE @sql NVARCHAR(MAX) = 'SELECT COUNT(*) ';
	
	IF @estado = 1 BEGIN 
		SET @sql = 'SELECT * ';
	END

	DECLARE @comprobacion INT = 1;

	DECLARE @tabla NVARCHAR(MAX) = (SELECT Tabla FROM @informacion);
	DECLARE @NRelaciones INT = (SELECT NRelaciones FROM @informacion);
	DECLARE @Campo NVARCHAR(MAX) = (SELECT Campo FROM @informacion);
	DECLARE @Valor NVARCHAR(MAX) = (SELECT Valor FROM @informacion);

	SET @sql += 'FROM ' + @tabla;

	IF @numeroLlaves > 1 
	BEGIN
		WHILE @minimo <= @maximo BEGIN 
	
			DECLARE @indices NVARCHAR(MAX);

			SELECT 
				@indices = (CASE
					WHEN COUNT(*) != 0 THEN CAST(@minimo AS NVARCHAR(MAX)) 
					ELSE 
						'error'
				END) 
			FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE as k
			WHERE EXISTS(
				SELECT * 
				FROM INFORMATION_SCHEMA.COLUMNS AS c
				WHERE c.ORDINAL_POSITION = @minimo AND k.COLUMN_NAME = c.COLUMN_NAME AND k.CONSTRAINT_NAME LIKE '%FK%' AND c.TABLE_NAME = @tabla
			);

			IF @indices != 'error' BEGIN 
				DECLARE @TablaSecundaria NVARCHAR(MAX), @Columna NVARCHAR(MAX);

				SELECT @TablaSecundaria = REPLACE(K.CONSTRAINT_NAME, 'FK_'+K.TABLE_NAME+'_', ''), 
					@Columna = K.COLUMN_NAME,
					@TablaSecundaria = REPLACE(@TablaSecundaria, '_'+K.COLUMN_NAME, '')
				FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE AS K 
					INNER JOIN INFORMATION_SCHEMA.COLUMNS AS C 
						ON K.COLUMN_NAME = C.COLUMN_NAME
				WHERE K.TABLE_NAME = @tabla AND K.CONSTRAINT_NAME LIKE '%FK%' AND C.ORDINAL_POSITION = CAST(@indices AS INT)
				ORDER BY C.ORDINAL_POSITION ASC;
				
				SELECT @sql += ' LEFT JOIN ' + @TablaSecundaria + ' ON (' + @tabla + '.' + @Columna + ' = ' + c.TABLE_NAME + '.' + c.COLUMN_NAME + ')'
				FROM INFORMATION_SCHEMA.COLUMNS as c
				WHERE EXISTS(
					SELECT * 
					FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE as k
					WHERE c.TABLE_NAME = @TablaSecundaria AND k.TABLE_NAME = @TablaSecundaria AND c.COLUMN_NAME = k.COLUMN_NAME AND k.CONSTRAINT_NAME LIKE '%PK%'
				);		

				--aqui comprobamos si la tabla secundaria tiene otras relaciones mas adicionales que realizarse.
				DECLARE @EXIT INT = 1;

				WHILE @EXIT = 1 BEGIN 
					DECLARE @inicio INT = (SELECT TOP 1 C.ORDINAL_POSITION
					FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE AS K
						INNER JOIN INFORMATION_SCHEMA.COLUMNS AS C
							ON k.COLUMN_NAME = C.COLUMN_NAME
					WHERE K.TABLE_NAME = @TablaSecundaria AND K.CONSTRAINT_NAME LIKE '%FK%' 
					ORDER BY C.ORDINAL_POSITION ASC);

					DECLARE @final INT = (SELECT TOP 1 C.ORDINAL_POSITION
					FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE AS K
						INNER JOIN INFORMATION_SCHEMA.COLUMNS AS C
							ON k.COLUMN_NAME = C.COLUMN_NAME
					WHERE K.TABLE_NAME = @TablaSecundaria AND K.CONSTRAINT_NAME LIKE '%FK%' 
					ORDER BY C.ORDINAL_POSITION DESC);

					DECLARE @actualizarTabla NVARCHAR(MAX) = 'sin_tabla';
					WHILE @inicio <= @final BEGIN 
						DECLARE @indices2 NVARCHAR(MAX);

						SELECT 
							@indices2 = (
								CASE
									WHEN COUNT(*) != 0 THEN CAST(@inicio AS NVARCHAR(MAX))
									ELSE 
										'error'
								END 
							)
						FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE as k
						WHERE EXISTS(
							SELECT * 
							FROM INFORMATION_SCHEMA.COLUMNS as c 
							WHERE k.TABLE_NAME = @TablaSecundaria AND c.TABLE_NAME = @TablaSecundaria AND k.CONSTRAINT_NAME LIKE '%FK%' AND c.ORDINAL_POSITION = @inicio
						);	
					
						IF @indices2 != 'error' BEGIN 
							DECLARE @otraTabla NVARCHAR(MAX), @Columna2 NVARCHAR(MAX);

							SELECT @otraTabla = REPLACE(K.CONSTRAINT_NAME, 'FK_'+K.TABLE_NAME+'_', ''), 
								@Columna2 = K.COLUMN_NAME,
								@otraTabla = REPLACE(@otraTabla, '_'+K.COLUMN_NAME, '')
							FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE AS K 
								INNER JOIN INFORMATION_SCHEMA.COLUMNS AS C 
									ON K.COLUMN_NAME = C.COLUMN_NAME
							WHERE K.TABLE_NAME = @TablaSecundaria AND K.CONSTRAINT_NAME LIKE '%FK%' AND C.ORDINAL_POSITION = CAST(@indices2 AS INT)
							ORDER BY C.ORDINAL_POSITION ASC;
				
							SELECT @sql += ' LEFT JOIN ' + @otraTabla + ' ON (' + @TablaSecundaria + '.' + @Columna2 + ' = ' + c.TABLE_NAME + '.' + c.COLUMN_NAME + ')'
							FROM INFORMATION_SCHEMA.COLUMNS as c
							WHERE EXISTS(
								SELECT * 
								FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE as k
								WHERE c.TABLE_NAME = @otraTabla AND k.TABLE_NAME = @otraTabla AND c.COLUMN_NAME = k.COLUMN_NAME AND k.CONSTRAINT_NAME LIKE '%PK%'
							);

							-- comprobar si otraTabla tiene alguna relacion con otra tabla ....
							IF EXISTS(SELECT * FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE AS K WHERE K.TABLE_NAME = @TablaSecundaria AND K.CONSTRAINT_NAME LIKE '%FK%') BEGIN 
								SET @actualizarTabla = @otraTabla
							END 
						END 

						SET @inicio += 1;
					END 

					IF (@actualizarTabla != 'sin_tabla') AND (@inicio != @final) BEGIN 
						-- actualizamos nuestar tabla secundaria 
						SET @TablaSecundaria = @actualizarTabla
					END 
					ELSE BEGIN 
						SET @EXIT = 0;
					END 

				END -- while = 1
			END 

			SET @minimo += 1;
		END
	END -- -

	IF @Campo != '' AND @Valor != '' 
	BEGIN 
		SET @sql += ' WHERE ' + dbo.FuncionString(@NRelaciones, @Campo, @Valor, 'verificar');
	END
	
	INSERT INTO @verificado
	SELECT @sql;

	RETURN;
END
GO
/****** Object:  Table [dbo].[Carroceria]    Script Date: 2/2/2024 12:31:11 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[Carroceria](
	[carroceria_id] [int] IDENTITY(1,1) NOT NULL,
	[carroceria_tipo] [nvarchar](90) NOT NULL,
 CONSTRAINT [PK_Carroceria] PRIMARY KEY CLUSTERED 
(
	[carroceria_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[Color]    Script Date: 2/2/2024 12:31:12 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[Color](
	[color_id] [int] IDENTITY(1,1) NOT NULL,
	[color_tipo] [nvarchar](20) NOT NULL,
 CONSTRAINT [PK_Color] PRIMARY KEY CLUSTERED 
(
	[color_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[Modelo]    Script Date: 2/2/2024 12:31:12 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[Modelo](
	[modelo_id] [int] IDENTITY(1,1) NOT NULL,
	[modelo_tipo] [nvarchar](80) NOT NULL,
 CONSTRAINT [PK_Modelo] PRIMARY KEY CLUSTERED 
(
	[modelo_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[Proveedor]    Script Date: 2/2/2024 12:31:12 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[Proveedor](
	[proveedor_id] [int] IDENTITY(1,1) NOT NULL,
	[proveedor_direccion] [nvarchar](max) NOT NULL,
	[proveedor_provincia] [nvarchar](max) NOT NULL,
	[proveedor_pais] [nvarchar](max) NOT NULL,
	[proveedor_nombreEmpresa] [nvarchar](max) NOT NULL,
 CONSTRAINT [PK_Proveedor] PRIMARY KEY CLUSTERED 
(
	[proveedor_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO
/****** Object:  Table [dbo].[Marca]    Script Date: 2/2/2024 12:31:12 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[Marca](
	[marca_id] [int] IDENTITY(1,1) NOT NULL,
	[marca_tipo] [nvarchar](80) NOT NULL,
	[marca_proveedorId] [int] NOT NULL,
 CONSTRAINT [PK_Marca] PRIMARY KEY CLUSTERED 
(
	[marca_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[Inventario]    Script Date: 2/2/2024 12:31:12 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[Inventario](
	[inventario_id] [int] IDENTITY(1,1) NOT NULL,
	[inventario_carroceriaId] [int] NOT NULL,
	[inventario_colorId] [int] NOT NULL,
	[inventario_modeloId] [int] NOT NULL,
	[inventario_marcaId] [int] NOT NULL,
	[inventario_unidades] [int] NOT NULL,
	[inventario_disponibilidad] [int] NOT NULL,
 CONSTRAINT [PK_Inventario] PRIMARY KEY CLUSTERED 
(
	[inventario_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[Auto]    Script Date: 2/2/2024 12:31:12 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[Auto](
	[auto_id] [int] IDENTITY(1,1) NOT NULL,
	[auto_puertas] [int] NOT NULL,
	[auto_placa] [nvarchar](9) NOT NULL,
	[auto_fechaInicioProduccion] [datetime2](7) NOT NULL,
	[auto_fechaFinalProduccion] [datetime2](7) NOT NULL,
	[auto_precio] [float] NULL,
	[auto_inventarioId] [int] NOT NULL,
 CONSTRAINT [PK_Auto] PRIMARY KEY CLUSTERED 
(
	[auto_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  View [dbo].[ViewInventario]    Script Date: 2/2/2024 12:31:12 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [dbo].[ViewInventario]
AS
	SELECT auto_id, auto_puertas, auto_placa, auto_fechaInicioProduccion, auto_fechaFinalProduccion, auto_precio, inventario_unidades, 
		CASE 
			WHEN inventario_disponibilidad = 1 THEN 'stock bajo'
			WHEN inventario_disponibilidad = 3 THEN 'stock alto'
			WHEN inventario_disponibilidad = 2 THEN 'stcok medio'
			ELSE 
				'stock vacio'
		END as tipo
		, carroceria_tipo, color_tipo, modelo_tipo, marca_tipo, proveedor_direccion, proveedor_provincia, proveedor_pais, proveedor_nombreEmpresa
	FROM Auto AS a
		INNER JOIN Inventario AS i
			ON a.auto_inventarioId = i.inventario_id
		INNER JOIN Carroceria AS c 
			ON i.inventario_carroceriaId = c.carroceria_id
		INNER JOIN Color AS cl
			ON i.inventario_colorId = cl.color_id
		INNER JOIN Modelo AS ml
			ON i.inventario_modeloId = ml.modelo_id
		INNER JOIN Marca AS m 
			ON i.inventario_marcaId = m.marca_id
		INNER JOIN Proveedor as pl
			ON m.marca_proveedorId = pl.proveedor_id
	;
GO
/****** Object:  Table [dbo].[MetodoPago]    Script Date: 2/2/2024 12:31:12 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[MetodoPago](
	[metodoPago_id] [int] IDENTITY(1,1) NOT NULL,
	[metodoPago_tipo] [int] NOT NULL,
	[metodoPago_estado] [int] NOT NULL,
 CONSTRAINT [PK_MetodoPago] PRIMARY KEY CLUSTERED 
(
	[metodoPago_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[Perfil]    Script Date: 2/2/2024 12:31:12 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[Perfil](
	[perfil_id] [int] IDENTITY(1,1) NOT NULL,
	[perfil_nombreRol] [nvarchar](50) NOT NULL,
	[perfil_estado] [bit] NOT NULL,
 CONSTRAINT [PK_Perfil] PRIMARY KEY CLUSTERED 
(
	[perfil_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[Usuario]    Script Date: 2/2/2024 12:31:12 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[Usuario](
	[usuario_id] [int] IDENTITY(1,1) NOT NULL,
	[usuario_nombre] [nvarchar](100) NOT NULL,
	[usuario_apellido] [nvarchar](100) NOT NULL,
	[usuario_cedula] [nvarchar](10) NOT NULL,
	[usuario_celular] [nvarchar](10) NOT NULL,
	[usuario_correo] [nvarchar](100) NOT NULL,
	[usuario_contrasena] [nvarchar](50) NOT NULL,
 CONSTRAINT [PK_Usuario] PRIMARY KEY CLUSTERED 
(
	[usuario_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[Compra]    Script Date: 2/2/2024 12:31:12 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[Compra](
	[compra_id] [int] IDENTITY(1,1) NOT NULL,
	[compra_fecha] [datetime2](7) NOT NULL,
	[compra_comprobanteDescripcion] [nvarchar](max) NOT NULL,
	[compra_comprobanteMonto] [float] NOT NULL,
	[compra_comprobanteEmpresa] [nvarchar](max) NOT NULL,
	[compra_estado] [int] NOT NULL,
	[compra_metodoPagoId] [int] NOT NULL,
 CONSTRAINT [PK_Compra] PRIMARY KEY CLUSTERED 
(
	[compra_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY] TEXTIMAGE_ON [PRIMARY]
GO
/****** Object:  Table [dbo].[UsuarioPerfil]    Script Date: 2/2/2024 12:31:12 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[UsuarioPerfil](
	[usuarioPerfil_id] [int] IDENTITY(1,1) NOT NULL,
	[usuarioPerfil_usuarioId] [int] NOT NULL,
	[usuarioPerfil_perfilId] [int] NOT NULL,
 CONSTRAINT [PK_UsuarioPerfil] PRIMARY KEY CLUSTERED 
(
	[usuarioPerfil_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[Reserva]    Script Date: 2/2/2024 12:31:12 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[Reserva](
	[reserva_id] [int] IDENTITY(1,1) NOT NULL,
	[reserva_compraId] [int] NULL,
	[reserva_pruebaManejoId] [int] NULL,
	[reserva_usuarioPerfilId] [int] NOT NULL,
	[reserva_estado] [int] NOT NULL,
	[reserva_AutoId] [int] NOT NULL,
	[reserva_asunto] [nvarchar](250) NOT NULL,
	[reserva_fecha] [datetime2](7) NOT NULL,
	[reserva_hora] [nvarchar](50) NOT NULL,
 CONSTRAINT [PK_Reserva] PRIMARY KEY CLUSTERED 
(
	[reserva_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  View [dbo].[ViewPago]    Script Date: 2/2/2024 12:31:12 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [dbo].[ViewPago]
AS
	SELECT us.usuario_nombre, us.usuario_apellido, us.usuario_cedula,
			us.usuario_celular, us.usuario_correo, pf.perfil_nombreRol, pf.perfil_estado,
			r.reserva_asunto, r.reserva_fecha, r.reserva_hora,
			c.compra_fecha, c.compra_comprobanteDescripcion, c.compra_comprobanteMonto,
			c.compra_comprobanteEmpresa, c.compra_estado, 
			CASE 
				WHEN c.compra_estado = 2 THEN 'pendiente'
				ELSE 
					'cancelado'
			END as tipo_actividad
			,
			CASE 
				WHEN mp.metodoPago_tipo = 1 THEN 'transferencia'
				ELSE 
					'tarjeta credito'
			END as tipoMetodoPago, 
			CASE 
				WHEN mp.metodoPago_estado = 1 THEN 'habilitado'
				ELSE 
					'deshabilitado'
			END as estado
	FROM Reserva AS r

		INNER JOIN UsuarioPerfil AS up
			ON r.reserva_usuarioPerfilId = up.usuarioPerfil_id

		INNER JOIN Usuario AS us 
			ON up.usuarioPerfil_usuarioId = us.usuario_id

		INNER JOIN Perfil AS pf
			ON up.usuarioPerfil_perfilId = pf.perfil_id
		
		INNER JOIN Auto as at 
			ON r.reserva_AutoId = at.auto_id
	
		INNER JOIN Compra AS c
			ON r.reserva_compraId = c.compra_id

		INNER JOIN MetodoPago AS mp 
			ON c.compra_metodoPagoId = mp.metodoPago_id
	;
GO
/****** Object:  View [dbo].[ViewCompra]    Script Date: 2/2/2024 12:31:12 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE VIEW [dbo].[ViewCompra]
AS
	SELECT compra_id, compra_fecha, compra_comprobanteDescripcion, compra_comprobanteMonto, compra_comprobanteEmpresa, cr.carroceria_tipo as carroceria, cl.color_tipo as color, ml.modelo_tipo as modelo, a.auto_placa, a.auto_precio as precioAuto,
			CASE 
				WHEN mp.metodoPago_tipo = 1 THEN 'transferencia'
				ELSE 
					'tarjeta credito' 
			END as modoPago, 
			CASE 
				WHEN mp.metodoPago_estado = 1 THEN 'habilitado'
				ELSE 
					'deshabilitado' 
			END as estadoPago
	FROM Compra AS c
	
		INNER JOIN MetodoPago AS mp 
			ON c.compra_metodoPagoId = mp.metodoPago_id

		INNER JOIN Reserva AS r
			ON r.reserva_compraId = c.compra_id

		INNER JOIN Auto AS a 
			ON r.reserva_AutoId = a.auto_id

		INNER JOIN Inventario AS i 
			ON a.auto_inventarioId = i.inventario_id

		INNER JOIN Carroceria AS cr 
			ON i.inventario_carroceriaId = cr.carroceria_id

		INNER JOIN Color AS cl
			ON i.inventario_colorId = cl.color_id

		INNER JOIN Modelo AS ml
			ON i.inventario_modeloId = ml.modelo_id

		INNER JOIN Marca AS m 
			ON i.inventario_marcaId = m.marca_id

		INNER JOIN Proveedor as pl
			ON m.marca_proveedorId = pl.proveedor_id
	
		;
GO
/****** Object:  Table [dbo].[__EFMigrationsHistory]    Script Date: 2/2/2024 12:31:12 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[__EFMigrationsHistory](
	[MigrationId] [nvarchar](150) NOT NULL,
	[ProductVersion] [nvarchar](32) NOT NULL,
 CONSTRAINT [PK___EFMigrationsHistory] PRIMARY KEY CLUSTERED 
(
	[MigrationId] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
/****** Object:  Table [dbo].[PruebaManejo]    Script Date: 2/2/2024 12:31:12 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE TABLE [dbo].[PruebaManejo](
	[pruebaManejo_id] [int] IDENTITY(1,1) NOT NULL,
	[pruebaManejo_estado] [int] NOT NULL,
	[pruebaManejo_descripcion] [nvarchar](300) NOT NULL,
	[pruebaManejo_nivelSatisfaccion] [int] NOT NULL,
 CONSTRAINT [PK_PruebaManejo] PRIMARY KEY CLUSTERED 
(
	[pruebaManejo_id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
) ON [PRIMARY]
GO
INSERT [dbo].[__EFMigrationsHistory] ([MigrationId], [ProductVersion]) VALUES (N'20240125091915_cambios_nuevaMigracion', N'7.0.14')
INSERT [dbo].[__EFMigrationsHistory] ([MigrationId], [ProductVersion]) VALUES (N'20240130045436_miMigracionNueva', N'7.0.14')
GO
SET IDENTITY_INSERT [dbo].[Auto] ON 

INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (1, 4, N'954B114', CAST(N'2005-02-12T00:00:00.0000000' AS DateTime2), CAST(N'2008-03-12T00:00:00.0000000' AS DateTime2), 330.2, 1)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (2, 4, N'D6331B4', CAST(N'2005-02-12T00:00:00.0000000' AS DateTime2), CAST(N'2008-03-12T00:00:00.0000000' AS DateTime2), 120.2, 1)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (3, 4, N'457F1E5', CAST(N'2005-02-12T00:00:00.0000000' AS DateTime2), CAST(N'2008-03-12T00:00:00.0000000' AS DateTime2), 200.2, 1)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (4, 4, N'F401462', CAST(N'2005-02-12T00:00:00.0000000' AS DateTime2), CAST(N'2008-03-12T00:00:00.0000000' AS DateTime2), 930.2, 1)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (5, 4, N'9D06F24', CAST(N'2005-02-12T00:00:00.0000000' AS DateTime2), CAST(N'2008-03-12T00:00:00.0000000' AS DateTime2), 120.2, 1)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (6, 4, N'8B3CAA7', CAST(N'2012-02-12T00:00:00.0000000' AS DateTime2), CAST(N'2013-03-12T00:00:00.0000000' AS DateTime2), 390.2, 2)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (7, 4, N'7F84B3A', CAST(N'2012-02-12T00:00:00.0000000' AS DateTime2), CAST(N'2013-03-12T00:00:00.0000000' AS DateTime2), 390.2, 2)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (8, 4, N'1BA1361', CAST(N'2012-02-12T00:00:00.0000000' AS DateTime2), CAST(N'2013-03-12T00:00:00.0000000' AS DateTime2), 390.2, 2)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (9, 4, N'C40B186', CAST(N'2012-02-12T00:00:00.0000000' AS DateTime2), CAST(N'2013-03-12T00:00:00.0000000' AS DateTime2), 390.2, 2)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (10, 4, N'D518982', CAST(N'2012-02-12T00:00:00.0000000' AS DateTime2), CAST(N'2013-03-12T00:00:00.0000000' AS DateTime2), 390.2, 2)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (11, 2, N'C014164', CAST(N'2001-02-12T00:00:00.0000000' AS DateTime2), CAST(N'2004-03-12T00:00:00.0000000' AS DateTime2), 390.2, 3)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (12, 2, N'7E89EA0', CAST(N'2001-02-12T00:00:00.0000000' AS DateTime2), CAST(N'2004-03-12T00:00:00.0000000' AS DateTime2), 390.2, 3)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (13, 2, N'CC79089', CAST(N'2001-02-12T00:00:00.0000000' AS DateTime2), CAST(N'2004-03-12T00:00:00.0000000' AS DateTime2), 390.2, 3)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (14, 2, N'743F5B3', CAST(N'2001-02-12T00:00:00.0000000' AS DateTime2), CAST(N'2004-03-12T00:00:00.0000000' AS DateTime2), 390.2, 3)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (15, 2, N'A68EB87', CAST(N'2001-02-12T00:00:00.0000000' AS DateTime2), CAST(N'2004-03-12T00:00:00.0000000' AS DateTime2), 390.2, 3)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (16, 4, N'C45D8B6', CAST(N'2019-02-12T00:00:00.0000000' AS DateTime2), CAST(N'2022-03-12T00:00:00.0000000' AS DateTime2), 99.2, 4)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (17, 4, N'403214D', CAST(N'2019-02-12T00:00:00.0000000' AS DateTime2), CAST(N'2022-03-12T00:00:00.0000000' AS DateTime2), 99.2, 4)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (18, 4, N'2AD33F9', CAST(N'2019-02-12T00:00:00.0000000' AS DateTime2), CAST(N'2022-03-12T00:00:00.0000000' AS DateTime2), 99.2, 4)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (19, 4, N'6FCDCF1', CAST(N'2019-02-12T00:00:00.0000000' AS DateTime2), CAST(N'2022-03-12T00:00:00.0000000' AS DateTime2), 99.2, 4)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (20, 4, N'DC440AB', CAST(N'2019-02-12T00:00:00.0000000' AS DateTime2), CAST(N'2022-03-12T00:00:00.0000000' AS DateTime2), 99.2, 4)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (21, 4, N'13A672F', CAST(N'2019-02-12T00:00:00.0000000' AS DateTime2), CAST(N'2022-03-12T00:00:00.0000000' AS DateTime2), 98.2, 5)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (22, 4, N'D25A735', CAST(N'2019-02-12T00:00:00.0000000' AS DateTime2), CAST(N'2022-03-12T00:00:00.0000000' AS DateTime2), 98.2, 5)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (23, 4, N'C6BA460', CAST(N'2019-02-12T00:00:00.0000000' AS DateTime2), CAST(N'2022-03-12T00:00:00.0000000' AS DateTime2), 98.2, 5)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (24, 4, N'2405482', CAST(N'2019-02-12T00:00:00.0000000' AS DateTime2), CAST(N'2022-03-12T00:00:00.0000000' AS DateTime2), 98.2, 5)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (25, 4, N'06BC83A', CAST(N'2019-02-12T00:00:00.0000000' AS DateTime2), CAST(N'2022-03-12T00:00:00.0000000' AS DateTime2), 98.2, 5)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (26, 4, N'C6133EE', CAST(N'2022-02-12T00:00:00.0000000' AS DateTime2), CAST(N'2023-03-12T00:00:00.0000000' AS DateTime2), 999.2, 6)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (27, 4, N'EEB9EA1', CAST(N'2022-02-12T00:00:00.0000000' AS DateTime2), CAST(N'2023-03-12T00:00:00.0000000' AS DateTime2), 999.2, 6)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (28, 4, N'3AAE8AD', CAST(N'2022-02-12T00:00:00.0000000' AS DateTime2), CAST(N'2023-03-12T00:00:00.0000000' AS DateTime2), 999.2, 6)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (29, 4, N'F35F408', CAST(N'2022-02-12T00:00:00.0000000' AS DateTime2), CAST(N'2023-03-12T00:00:00.0000000' AS DateTime2), 999.2, 6)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (30, 4, N'F51D514', CAST(N'2022-02-12T00:00:00.0000000' AS DateTime2), CAST(N'2023-03-12T00:00:00.0000000' AS DateTime2), 999.2, 6)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (31, 4, N'8E9FADA', CAST(N'2019-01-11T00:00:00.0000000' AS DateTime2), CAST(N'2021-11-11T00:00:00.0000000' AS DateTime2), 120.2, 7)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (32, 4, N'1113B0F', CAST(N'2019-01-11T00:00:00.0000000' AS DateTime2), CAST(N'2021-11-11T00:00:00.0000000' AS DateTime2), 120.2, 7)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (33, 4, N'10E45E3', CAST(N'2019-01-11T00:00:00.0000000' AS DateTime2), CAST(N'2021-11-11T00:00:00.0000000' AS DateTime2), 120.2, 7)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (34, 4, N'B86EEE5', CAST(N'2019-01-11T00:00:00.0000000' AS DateTime2), CAST(N'2021-11-11T00:00:00.0000000' AS DateTime2), 120.2, 7)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (35, 4, N'443E7A9', CAST(N'2019-01-11T00:00:00.0000000' AS DateTime2), CAST(N'2021-11-11T00:00:00.0000000' AS DateTime2), 120.2, 7)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (36, 4, N'C0E894B', CAST(N'2022-11-11T00:00:00.0000000' AS DateTime2), CAST(N'2024-02-11T00:00:00.0000000' AS DateTime2), 200.2, 8)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (37, 4, N'A570649', CAST(N'2022-11-11T00:00:00.0000000' AS DateTime2), CAST(N'2024-02-11T00:00:00.0000000' AS DateTime2), 200.2, 8)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (38, 4, N'A7E8877', CAST(N'2022-11-11T00:00:00.0000000' AS DateTime2), CAST(N'2024-02-11T00:00:00.0000000' AS DateTime2), 200.2, 8)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (39, 4, N'601F331', CAST(N'2022-11-11T00:00:00.0000000' AS DateTime2), CAST(N'2024-02-11T00:00:00.0000000' AS DateTime2), 200.2, 8)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (40, 4, N'A5B6CF3', CAST(N'2022-11-11T00:00:00.0000000' AS DateTime2), CAST(N'2024-02-11T00:00:00.0000000' AS DateTime2), 200.2, 8)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (41, 4, N'59596EB', CAST(N'2022-06-02T00:00:00.0000000' AS DateTime2), CAST(N'2024-08-10T00:00:00.0000000' AS DateTime2), 225.2, 9)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (42, 4, N'8D3A6FD', CAST(N'2022-06-02T00:00:00.0000000' AS DateTime2), CAST(N'2024-08-10T00:00:00.0000000' AS DateTime2), 225.2, 9)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (43, 4, N'BE15B9D', CAST(N'2022-06-02T00:00:00.0000000' AS DateTime2), CAST(N'2024-08-10T00:00:00.0000000' AS DateTime2), 225.2, 9)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (44, 4, N'9177B73', CAST(N'2022-06-02T00:00:00.0000000' AS DateTime2), CAST(N'2024-08-10T00:00:00.0000000' AS DateTime2), 225.2, 9)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (45, 4, N'EB3590D', CAST(N'2022-06-02T00:00:00.0000000' AS DateTime2), CAST(N'2024-08-10T00:00:00.0000000' AS DateTime2), 225.2, 9)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (46, 4, N'2D4EF13', CAST(N'2020-02-02T00:00:00.0000000' AS DateTime2), CAST(N'2022-05-10T00:00:00.0000000' AS DateTime2), 205.2, 10)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (47, 4, N'9167340', CAST(N'2020-02-02T00:00:00.0000000' AS DateTime2), CAST(N'2022-05-10T00:00:00.0000000' AS DateTime2), 205.2, 10)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (48, 4, N'7AB76DA', CAST(N'2020-02-02T00:00:00.0000000' AS DateTime2), CAST(N'2022-05-10T00:00:00.0000000' AS DateTime2), 205.2, 10)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (49, 4, N'525B507', CAST(N'2020-02-02T00:00:00.0000000' AS DateTime2), CAST(N'2022-05-10T00:00:00.0000000' AS DateTime2), 205.2, 10)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (50, 4, N'BED3E62', CAST(N'2020-02-02T00:00:00.0000000' AS DateTime2), CAST(N'2022-05-10T00:00:00.0000000' AS DateTime2), 205.2, 10)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (51, 4, N'829BE85', CAST(N'2010-02-02T00:00:00.0000000' AS DateTime2), CAST(N'2012-05-10T00:00:00.0000000' AS DateTime2), 105.2, 11)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (52, 4, N'CFA34D5', CAST(N'2010-02-02T00:00:00.0000000' AS DateTime2), CAST(N'2012-05-10T00:00:00.0000000' AS DateTime2), 105.2, 11)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (53, 4, N'1919000', CAST(N'2010-02-02T00:00:00.0000000' AS DateTime2), CAST(N'2012-05-10T00:00:00.0000000' AS DateTime2), 105.2, 11)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (54, 4, N'7C0C45B', CAST(N'2010-02-02T00:00:00.0000000' AS DateTime2), CAST(N'2012-05-10T00:00:00.0000000' AS DateTime2), 105.2, 11)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (55, 4, N'3180D35', CAST(N'2010-02-02T00:00:00.0000000' AS DateTime2), CAST(N'2012-05-10T00:00:00.0000000' AS DateTime2), 105.2, 11)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (56, 4, N'3B0D19A', CAST(N'2015-02-02T00:00:00.0000000' AS DateTime2), CAST(N'2017-05-10T00:00:00.0000000' AS DateTime2), 115.2, 12)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (57, 4, N'3177EAD', CAST(N'2015-02-02T00:00:00.0000000' AS DateTime2), CAST(N'2017-05-10T00:00:00.0000000' AS DateTime2), 115.2, 12)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (58, 4, N'48CD0D4', CAST(N'2015-02-02T00:00:00.0000000' AS DateTime2), CAST(N'2017-05-10T00:00:00.0000000' AS DateTime2), 115.2, 12)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (59, 4, N'0B958AE', CAST(N'2015-02-02T00:00:00.0000000' AS DateTime2), CAST(N'2017-05-10T00:00:00.0000000' AS DateTime2), 115.2, 12)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (60, 4, N'238E7BC', CAST(N'2015-02-02T00:00:00.0000000' AS DateTime2), CAST(N'2017-05-10T00:00:00.0000000' AS DateTime2), 115.2, 12)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (61, 4, N'3DCAAF9', CAST(N'2014-02-02T00:00:00.0000000' AS DateTime2), CAST(N'2017-05-10T00:00:00.0000000' AS DateTime2), 119.2, 13)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (62, 4, N'E58EB2B', CAST(N'2014-02-02T00:00:00.0000000' AS DateTime2), CAST(N'2017-05-10T00:00:00.0000000' AS DateTime2), 119.2, 13)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (63, 4, N'0999A3F', CAST(N'2014-02-02T00:00:00.0000000' AS DateTime2), CAST(N'2017-05-10T00:00:00.0000000' AS DateTime2), 119.2, 13)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (64, 4, N'283C364', CAST(N'2014-02-02T00:00:00.0000000' AS DateTime2), CAST(N'2017-05-10T00:00:00.0000000' AS DateTime2), 119.2, 13)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (65, 4, N'CF4D92A', CAST(N'2014-02-02T00:00:00.0000000' AS DateTime2), CAST(N'2017-05-10T00:00:00.0000000' AS DateTime2), 119.2, 13)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (66, 4, N'B53AC3D', CAST(N'2010-02-02T00:00:00.0000000' AS DateTime2), CAST(N'2012-05-10T00:00:00.0000000' AS DateTime2), 129.2, 14)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (67, 4, N'9B42CB6', CAST(N'2010-02-02T00:00:00.0000000' AS DateTime2), CAST(N'2012-05-10T00:00:00.0000000' AS DateTime2), 129.2, 14)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (68, 4, N'7760E93', CAST(N'2010-02-02T00:00:00.0000000' AS DateTime2), CAST(N'2012-05-10T00:00:00.0000000' AS DateTime2), 129.2, 14)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (69, 4, N'1EB7B11', CAST(N'2010-02-02T00:00:00.0000000' AS DateTime2), CAST(N'2012-05-10T00:00:00.0000000' AS DateTime2), 129.2, 14)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (70, 4, N'45AA55D', CAST(N'2010-02-02T00:00:00.0000000' AS DateTime2), CAST(N'2012-05-10T00:00:00.0000000' AS DateTime2), 129.2, 14)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (71, 4, N'DC7ACE8', CAST(N'2016-09-02T00:00:00.0000000' AS DateTime2), CAST(N'2018-11-10T00:00:00.0000000' AS DateTime2), 129.2, 15)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (72, 4, N'F7D02A6', CAST(N'2016-09-02T00:00:00.0000000' AS DateTime2), CAST(N'2018-11-10T00:00:00.0000000' AS DateTime2), 129.2, 15)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (73, 4, N'59F1955', CAST(N'2016-09-02T00:00:00.0000000' AS DateTime2), CAST(N'2018-11-10T00:00:00.0000000' AS DateTime2), 129.2, 15)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (74, 4, N'5EBB2E8', CAST(N'2016-09-02T00:00:00.0000000' AS DateTime2), CAST(N'2018-11-10T00:00:00.0000000' AS DateTime2), 129.2, 15)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (75, 4, N'9FA649A', CAST(N'2016-09-02T00:00:00.0000000' AS DateTime2), CAST(N'2018-11-10T00:00:00.0000000' AS DateTime2), 129.2, 15)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (76, 4, N'45F0055', CAST(N'2019-01-11T00:00:00.0000000' AS DateTime2), CAST(N'2020-02-12T00:00:00.0000000' AS DateTime2), 929.2, 16)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (77, 4, N'FA85274', CAST(N'2019-01-11T00:00:00.0000000' AS DateTime2), CAST(N'2020-02-12T00:00:00.0000000' AS DateTime2), 929.2, 16)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (78, 4, N'96B673A', CAST(N'2019-01-11T00:00:00.0000000' AS DateTime2), CAST(N'2020-02-12T00:00:00.0000000' AS DateTime2), 929.2, 16)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (79, 4, N'9A3D91E', CAST(N'2019-01-11T00:00:00.0000000' AS DateTime2), CAST(N'2020-02-12T00:00:00.0000000' AS DateTime2), 929.2, 16)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (80, 4, N'79DF225', CAST(N'2019-01-11T00:00:00.0000000' AS DateTime2), CAST(N'2020-02-12T00:00:00.0000000' AS DateTime2), 929.2, 16)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (81, 4, N'D21EA60', CAST(N'2021-01-11T00:00:00.0000000' AS DateTime2), CAST(N'2022-02-12T00:00:00.0000000' AS DateTime2), 900.2, 17)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (82, 4, N'0F4911C', CAST(N'2021-01-11T00:00:00.0000000' AS DateTime2), CAST(N'2022-02-12T00:00:00.0000000' AS DateTime2), 900.2, 17)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (83, 4, N'E40FD34', CAST(N'2021-01-11T00:00:00.0000000' AS DateTime2), CAST(N'2022-02-12T00:00:00.0000000' AS DateTime2), 900.2, 17)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (84, 4, N'086B64D', CAST(N'2021-01-11T00:00:00.0000000' AS DateTime2), CAST(N'2022-02-12T00:00:00.0000000' AS DateTime2), 900.2, 17)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (85, 4, N'B9DFF57', CAST(N'2021-01-11T00:00:00.0000000' AS DateTime2), CAST(N'2022-02-12T00:00:00.0000000' AS DateTime2), 900.2, 17)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (86, 4, N'80692CA', CAST(N'2019-01-09T00:00:00.0000000' AS DateTime2), CAST(N'2020-05-12T00:00:00.0000000' AS DateTime2), 200.2, 18)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (87, 4, N'F6E8DF5', CAST(N'2019-01-09T00:00:00.0000000' AS DateTime2), CAST(N'2020-05-12T00:00:00.0000000' AS DateTime2), 200.2, 18)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (88, 4, N'E020959', CAST(N'2019-01-09T00:00:00.0000000' AS DateTime2), CAST(N'2020-05-12T00:00:00.0000000' AS DateTime2), 200.2, 18)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (89, 4, N'68861B5', CAST(N'2019-01-09T00:00:00.0000000' AS DateTime2), CAST(N'2020-05-12T00:00:00.0000000' AS DateTime2), 200.2, 18)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (90, 4, N'3C93535', CAST(N'2019-01-09T00:00:00.0000000' AS DateTime2), CAST(N'2020-05-12T00:00:00.0000000' AS DateTime2), 200.2, 18)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (91, 4, N'068AB21', CAST(N'2020-01-09T00:00:00.0000000' AS DateTime2), CAST(N'2021-05-12T00:00:00.0000000' AS DateTime2), 250.2, 19)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (92, 4, N'16C24DF', CAST(N'2020-01-09T00:00:00.0000000' AS DateTime2), CAST(N'2021-05-12T00:00:00.0000000' AS DateTime2), 250.2, 19)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (93, 4, N'0FA8356', CAST(N'2020-01-09T00:00:00.0000000' AS DateTime2), CAST(N'2021-05-12T00:00:00.0000000' AS DateTime2), 250.2, 19)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (94, 4, N'0ACAD94', CAST(N'2020-01-09T00:00:00.0000000' AS DateTime2), CAST(N'2021-05-12T00:00:00.0000000' AS DateTime2), 250.2, 19)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (95, 4, N'263FE3F', CAST(N'2020-01-09T00:00:00.0000000' AS DateTime2), CAST(N'2021-05-12T00:00:00.0000000' AS DateTime2), 250.2, 19)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (96, 4, N'47DE226', CAST(N'2019-01-09T00:00:00.0000000' AS DateTime2), CAST(N'2020-05-12T00:00:00.0000000' AS DateTime2), 350.2, 20)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (97, 4, N'C692612', CAST(N'2019-01-09T00:00:00.0000000' AS DateTime2), CAST(N'2020-05-12T00:00:00.0000000' AS DateTime2), 350.2, 20)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (98, 4, N'3FD76ED', CAST(N'2019-01-09T00:00:00.0000000' AS DateTime2), CAST(N'2020-05-12T00:00:00.0000000' AS DateTime2), 350.2, 20)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (99, 4, N'B5D8A8A', CAST(N'2019-01-09T00:00:00.0000000' AS DateTime2), CAST(N'2020-05-12T00:00:00.0000000' AS DateTime2), 350.2, 20)
GO
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (100, 4, N'FB47882', CAST(N'2019-01-09T00:00:00.0000000' AS DateTime2), CAST(N'2020-05-12T00:00:00.0000000' AS DateTime2), 350.2, 20)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (106, 2, N'JF90JIO2', CAST(N'2023-09-12T00:00:00.0000000' AS DateTime2), CAST(N'2023-07-11T00:00:00.0000000' AS DateTime2), 120.2, 1)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (107, 4, N'JFJJ9111', CAST(N'2023-09-12T00:00:00.0000000' AS DateTime2), CAST(N'2023-07-11T00:00:00.0000000' AS DateTime2), 140.2, 1)
INSERT [dbo].[Auto] ([auto_id], [auto_puertas], [auto_placa], [auto_fechaInicioProduccion], [auto_fechaFinalProduccion], [auto_precio], [auto_inventarioId]) VALUES (108, 2, N'J22TT111', CAST(N'2023-09-12T00:00:00.0000000' AS DateTime2), CAST(N'2023-07-11T00:00:00.0000000' AS DateTime2), 120.2, 1)
SET IDENTITY_INSERT [dbo].[Auto] OFF
GO
SET IDENTITY_INSERT [dbo].[Carroceria] ON 

INSERT [dbo].[Carroceria] ([carroceria_id], [carroceria_tipo]) VALUES (1, N'Sedán')
INSERT [dbo].[Carroceria] ([carroceria_id], [carroceria_tipo]) VALUES (2, N'Coupé')
INSERT [dbo].[Carroceria] ([carroceria_id], [carroceria_tipo]) VALUES (3, N'Hatchback')
INSERT [dbo].[Carroceria] ([carroceria_id], [carroceria_tipo]) VALUES (4, N'Descapotable o Convertible')
INSERT [dbo].[Carroceria] ([carroceria_id], [carroceria_tipo]) VALUES (5, N'SUV (Vehículo Utilitario Deportivo)')
INSERT [dbo].[Carroceria] ([carroceria_id], [carroceria_tipo]) VALUES (6, N'Monovolumen o Minivan')
INSERT [dbo].[Carroceria] ([carroceria_id], [carroceria_tipo]) VALUES (7, N'Pick-up')
INSERT [dbo].[Carroceria] ([carroceria_id], [carroceria_tipo]) VALUES (8, N'Furgoneta')
INSERT [dbo].[Carroceria] ([carroceria_id], [carroceria_tipo]) VALUES (9, N'Camioneta')
INSERT [dbo].[Carroceria] ([carroceria_id], [carroceria_tipo]) VALUES (10, N'Todoterreno o 4x4')
INSERT [dbo].[Carroceria] ([carroceria_id], [carroceria_tipo]) VALUES (11, N'Deportivo')
INSERT [dbo].[Carroceria] ([carroceria_id], [carroceria_tipo]) VALUES (12, N'Familiar')
INSERT [dbo].[Carroceria] ([carroceria_id], [carroceria_tipo]) VALUES (13, N'Crossover')
INSERT [dbo].[Carroceria] ([carroceria_id], [carroceria_tipo]) VALUES (14, N'Roadster')
INSERT [dbo].[Carroceria] ([carroceria_id], [carroceria_tipo]) VALUES (15, N'Berlina')
INSERT [dbo].[Carroceria] ([carroceria_id], [carroceria_tipo]) VALUES (16, N'Ranchera')
INSERT [dbo].[Carroceria] ([carroceria_id], [carroceria_tipo]) VALUES (17, N'Coupe-SUV')
INSERT [dbo].[Carroceria] ([carroceria_id], [carroceria_tipo]) VALUES (18, N'Fastback')
INSERT [dbo].[Carroceria] ([carroceria_id], [carroceria_tipo]) VALUES (19, N'Limusina')
INSERT [dbo].[Carroceria] ([carroceria_id], [carroceria_tipo]) VALUES (20, N'Microcoche')
INSERT [dbo].[Carroceria] ([carroceria_id], [carroceria_tipo]) VALUES (24, N'carroceria actualizado')
SET IDENTITY_INSERT [dbo].[Carroceria] OFF
GO
SET IDENTITY_INSERT [dbo].[Color] ON 

INSERT [dbo].[Color] ([color_id], [color_tipo]) VALUES (1, N'rojo')
INSERT [dbo].[Color] ([color_id], [color_tipo]) VALUES (2, N'azul')
INSERT [dbo].[Color] ([color_id], [color_tipo]) VALUES (3, N'verde')
INSERT [dbo].[Color] ([color_id], [color_tipo]) VALUES (4, N'amarillo')
SET IDENTITY_INSERT [dbo].[Color] OFF
GO
SET IDENTITY_INSERT [dbo].[Compra] ON 

INSERT [dbo].[Compra] ([compra_id], [compra_fecha], [compra_comprobanteDescripcion], [compra_comprobanteMonto], [compra_comprobanteEmpresa], [compra_estado], [compra_metodoPagoId]) VALUES (1, CAST(N'2024-02-22T00:00:00.0000000' AS DateTime2), N'compra final del automovil', 455.39, N'EMPRESA PRESTIGERACE', 1, 1)
INSERT [dbo].[Compra] ([compra_id], [compra_fecha], [compra_comprobanteDescripcion], [compra_comprobanteMonto], [compra_comprobanteEmpresa], [compra_estado], [compra_metodoPagoId]) VALUES (2, CAST(N'2024-01-15T00:00:00.0000000' AS DateTime2), N'compra final de auto ultimo modelo', 900.1, N'EMPRESA PRESTIGERACE', 1, 1)
INSERT [dbo].[Compra] ([compra_id], [compra_fecha], [compra_comprobanteDescripcion], [compra_comprobanteMonto], [compra_comprobanteEmpresa], [compra_estado], [compra_metodoPagoId]) VALUES (3, CAST(N'2024-01-20T00:00:00.0000000' AS DateTime2), N'compra automovil', 560.903, N'EMPRESA PRESTIGERACE', 1, 1)
INSERT [dbo].[Compra] ([compra_id], [compra_fecha], [compra_comprobanteDescripcion], [compra_comprobanteMonto], [compra_comprobanteEmpresa], [compra_estado], [compra_metodoPagoId]) VALUES (4, CAST(N'2023-02-22T00:00:00.0000000' AS DateTime2), N'compra final del automovil', 555.39, N'EMPRESA PRESTIGERACE', 1, 1)
INSERT [dbo].[Compra] ([compra_id], [compra_fecha], [compra_comprobanteDescripcion], [compra_comprobanteMonto], [compra_comprobanteEmpresa], [compra_estado], [compra_metodoPagoId]) VALUES (5, CAST(N'2023-01-15T00:00:00.0000000' AS DateTime2), N'compra final de auto ultimo modelo', 800.1, N'EMPRESA PRESTIGERACE', 1, 1)
INSERT [dbo].[Compra] ([compra_id], [compra_fecha], [compra_comprobanteDescripcion], [compra_comprobanteMonto], [compra_comprobanteEmpresa], [compra_estado], [compra_metodoPagoId]) VALUES (6, CAST(N'2023-01-20T00:00:00.0000000' AS DateTime2), N'compra automovil de la mejor calidad', 660.903, N'EMPRESA PRESTIGERACE', 1, 1)
INSERT [dbo].[Compra] ([compra_id], [compra_fecha], [compra_comprobanteDescripcion], [compra_comprobanteMonto], [compra_comprobanteEmpresa], [compra_estado], [compra_metodoPagoId]) VALUES (7, CAST(N'2023-12-22T00:00:00.0000000' AS DateTime2), N'compra final del automovil', 295.39, N'EMPRESA PRESTIGERACE', 1, 1)
INSERT [dbo].[Compra] ([compra_id], [compra_fecha], [compra_comprobanteDescripcion], [compra_comprobanteMonto], [compra_comprobanteEmpresa], [compra_estado], [compra_metodoPagoId]) VALUES (8, CAST(N'2023-12-15T00:00:00.0000000' AS DateTime2), N'compra final de auto ultimo modelo', 870.1, N'EMPRESA PRESTIGERACE', 1, 1)
INSERT [dbo].[Compra] ([compra_id], [compra_fecha], [compra_comprobanteDescripcion], [compra_comprobanteMonto], [compra_comprobanteEmpresa], [compra_estado], [compra_metodoPagoId]) VALUES (9, CAST(N'2023-12-20T00:00:00.0000000' AS DateTime2), N'compra automovil de la mejor calidad', 560.903, N'EMPRESA PRESTIGERACE', 1, 1)
SET IDENTITY_INSERT [dbo].[Compra] OFF
GO
SET IDENTITY_INSERT [dbo].[Inventario] ON 

INSERT [dbo].[Inventario] ([inventario_id], [inventario_carroceriaId], [inventario_colorId], [inventario_modeloId], [inventario_marcaId], [inventario_unidades], [inventario_disponibilidad]) VALUES (1, 1, 2, 1, 1, 5, 1)
INSERT [dbo].[Inventario] ([inventario_id], [inventario_carroceriaId], [inventario_colorId], [inventario_modeloId], [inventario_marcaId], [inventario_unidades], [inventario_disponibilidad]) VALUES (2, 1, 2, 2, 2, 5, 1)
INSERT [dbo].[Inventario] ([inventario_id], [inventario_carroceriaId], [inventario_colorId], [inventario_modeloId], [inventario_marcaId], [inventario_unidades], [inventario_disponibilidad]) VALUES (3, 2, 2, 3, 3, 5, 1)
INSERT [dbo].[Inventario] ([inventario_id], [inventario_carroceriaId], [inventario_colorId], [inventario_modeloId], [inventario_marcaId], [inventario_unidades], [inventario_disponibilidad]) VALUES (4, 1, 2, 4, 4, 5, 1)
INSERT [dbo].[Inventario] ([inventario_id], [inventario_carroceriaId], [inventario_colorId], [inventario_modeloId], [inventario_marcaId], [inventario_unidades], [inventario_disponibilidad]) VALUES (5, 1, 3, 5, 5, 5, 1)
INSERT [dbo].[Inventario] ([inventario_id], [inventario_carroceriaId], [inventario_colorId], [inventario_modeloId], [inventario_marcaId], [inventario_unidades], [inventario_disponibilidad]) VALUES (6, 1, 3, 6, 6, 5, 1)
INSERT [dbo].[Inventario] ([inventario_id], [inventario_carroceriaId], [inventario_colorId], [inventario_modeloId], [inventario_marcaId], [inventario_unidades], [inventario_disponibilidad]) VALUES (7, 1, 3, 7, 7, 5, 1)
INSERT [dbo].[Inventario] ([inventario_id], [inventario_carroceriaId], [inventario_colorId], [inventario_modeloId], [inventario_marcaId], [inventario_unidades], [inventario_disponibilidad]) VALUES (8, 1, 4, 8, 8, 5, 1)
INSERT [dbo].[Inventario] ([inventario_id], [inventario_carroceriaId], [inventario_colorId], [inventario_modeloId], [inventario_marcaId], [inventario_unidades], [inventario_disponibilidad]) VALUES (9, 1, 4, 9, 9, 5, 1)
INSERT [dbo].[Inventario] ([inventario_id], [inventario_carroceriaId], [inventario_colorId], [inventario_modeloId], [inventario_marcaId], [inventario_unidades], [inventario_disponibilidad]) VALUES (10, 1, 4, 10, 10, 5, 1)
INSERT [dbo].[Inventario] ([inventario_id], [inventario_carroceriaId], [inventario_colorId], [inventario_modeloId], [inventario_marcaId], [inventario_unidades], [inventario_disponibilidad]) VALUES (11, 5, 4, 11, 11, 5, 1)
INSERT [dbo].[Inventario] ([inventario_id], [inventario_carroceriaId], [inventario_colorId], [inventario_modeloId], [inventario_marcaId], [inventario_unidades], [inventario_disponibilidad]) VALUES (12, 5, 4, 12, 12, 5, 1)
INSERT [dbo].[Inventario] ([inventario_id], [inventario_carroceriaId], [inventario_colorId], [inventario_modeloId], [inventario_marcaId], [inventario_unidades], [inventario_disponibilidad]) VALUES (13, 5, 4, 13, 13, 5, 1)
INSERT [dbo].[Inventario] ([inventario_id], [inventario_carroceriaId], [inventario_colorId], [inventario_modeloId], [inventario_marcaId], [inventario_unidades], [inventario_disponibilidad]) VALUES (14, 5, 1, 14, 3, 5, 1)
INSERT [dbo].[Inventario] ([inventario_id], [inventario_carroceriaId], [inventario_colorId], [inventario_modeloId], [inventario_marcaId], [inventario_unidades], [inventario_disponibilidad]) VALUES (15, 5, 1, 15, 2, 5, 1)
INSERT [dbo].[Inventario] ([inventario_id], [inventario_carroceriaId], [inventario_colorId], [inventario_modeloId], [inventario_marcaId], [inventario_unidades], [inventario_disponibilidad]) VALUES (16, 5, 1, 16, 1, 5, 1)
INSERT [dbo].[Inventario] ([inventario_id], [inventario_carroceriaId], [inventario_colorId], [inventario_modeloId], [inventario_marcaId], [inventario_unidades], [inventario_disponibilidad]) VALUES (17, 5, 1, 17, 4, 5, 1)
INSERT [dbo].[Inventario] ([inventario_id], [inventario_carroceriaId], [inventario_colorId], [inventario_modeloId], [inventario_marcaId], [inventario_unidades], [inventario_disponibilidad]) VALUES (18, 5, 1, 18, 5, 5, 1)
INSERT [dbo].[Inventario] ([inventario_id], [inventario_carroceriaId], [inventario_colorId], [inventario_modeloId], [inventario_marcaId], [inventario_unidades], [inventario_disponibilidad]) VALUES (19, 5, 1, 19, 17, 5, 1)
INSERT [dbo].[Inventario] ([inventario_id], [inventario_carroceriaId], [inventario_colorId], [inventario_modeloId], [inventario_marcaId], [inventario_unidades], [inventario_disponibilidad]) VALUES (20, 1, 2, 20, 14, 5, 1)
INSERT [dbo].[Inventario] ([inventario_id], [inventario_carroceriaId], [inventario_colorId], [inventario_modeloId], [inventario_marcaId], [inventario_unidades], [inventario_disponibilidad]) VALUES (22, 1, 1, 2, 3, 1, 1)
SET IDENTITY_INSERT [dbo].[Inventario] OFF
GO
SET IDENTITY_INSERT [dbo].[Marca] ON 

INSERT [dbo].[Marca] ([marca_id], [marca_tipo], [marca_proveedorId]) VALUES (1, N'Honda', 1)
INSERT [dbo].[Marca] ([marca_id], [marca_tipo], [marca_proveedorId]) VALUES (2, N'Toyota', 1)
INSERT [dbo].[Marca] ([marca_id], [marca_tipo], [marca_proveedorId]) VALUES (3, N'Ford', 1)
INSERT [dbo].[Marca] ([marca_id], [marca_tipo], [marca_proveedorId]) VALUES (4, N'Chevrolet', 1)
INSERT [dbo].[Marca] ([marca_id], [marca_tipo], [marca_proveedorId]) VALUES (5, N'Nissan', 1)
INSERT [dbo].[Marca] ([marca_id], [marca_tipo], [marca_proveedorId]) VALUES (6, N'BMW', 1)
INSERT [dbo].[Marca] ([marca_id], [marca_tipo], [marca_proveedorId]) VALUES (7, N'Mercedes-Benz', 1)
INSERT [dbo].[Marca] ([marca_id], [marca_tipo], [marca_proveedorId]) VALUES (8, N'Volkswagen', 1)
INSERT [dbo].[Marca] ([marca_id], [marca_tipo], [marca_proveedorId]) VALUES (9, N'Audi', 1)
INSERT [dbo].[Marca] ([marca_id], [marca_tipo], [marca_proveedorId]) VALUES (10, N'Hyundai', 1)
INSERT [dbo].[Marca] ([marca_id], [marca_tipo], [marca_proveedorId]) VALUES (11, N'Subaru', 1)
INSERT [dbo].[Marca] ([marca_id], [marca_tipo], [marca_proveedorId]) VALUES (12, N'Mazda', 1)
INSERT [dbo].[Marca] ([marca_id], [marca_tipo], [marca_proveedorId]) VALUES (13, N'Jeep', 1)
INSERT [dbo].[Marca] ([marca_id], [marca_tipo], [marca_proveedorId]) VALUES (14, N'Tesla', 1)
INSERT [dbo].[Marca] ([marca_id], [marca_tipo], [marca_proveedorId]) VALUES (15, N'GMC', 1)
INSERT [dbo].[Marca] ([marca_id], [marca_tipo], [marca_proveedorId]) VALUES (16, N'Dodge', 1)
INSERT [dbo].[Marca] ([marca_id], [marca_tipo], [marca_proveedorId]) VALUES (17, N'Kia', 1)
INSERT [dbo].[Marca] ([marca_id], [marca_tipo], [marca_proveedorId]) VALUES (18, N'Volvo', 1)
INSERT [dbo].[Marca] ([marca_id], [marca_tipo], [marca_proveedorId]) VALUES (19, N'Jaguar', 1)
INSERT [dbo].[Marca] ([marca_id], [marca_tipo], [marca_proveedorId]) VALUES (20, N'Porsche', 1)
INSERT [dbo].[Marca] ([marca_id], [marca_tipo], [marca_proveedorId]) VALUES (21, N'Lexus', 1)
INSERT [dbo].[Marca] ([marca_id], [marca_tipo], [marca_proveedorId]) VALUES (22, N'Cadillac', 1)
INSERT [dbo].[Marca] ([marca_id], [marca_tipo], [marca_proveedorId]) VALUES (23, N'Infiniti', 1)
INSERT [dbo].[Marca] ([marca_id], [marca_tipo], [marca_proveedorId]) VALUES (24, N'Land Rover', 1)
SET IDENTITY_INSERT [dbo].[Marca] OFF
GO
SET IDENTITY_INSERT [dbo].[MetodoPago] ON 

INSERT [dbo].[MetodoPago] ([metodoPago_id], [metodoPago_tipo], [metodoPago_estado]) VALUES (1, 1, 1)
INSERT [dbo].[MetodoPago] ([metodoPago_id], [metodoPago_tipo], [metodoPago_estado]) VALUES (2, 2, 1)
SET IDENTITY_INSERT [dbo].[MetodoPago] OFF
GO
SET IDENTITY_INSERT [dbo].[Modelo] ON 

INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (1, N'Accord')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (2, N'Camry')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (3, N'Mustang')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (4, N'Malibu')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (5, N'Altima')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (6, N'3 Series')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (7, N'C-Class')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (8, N'Passat')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (9, N'A4')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (10, N'Sonata')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (11, N'Outback')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (12, N'CX-5')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (13, N'Grand Cherokee')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (14, N'Explorer')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (15, N'RAV4')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (16, N'CR-V')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (17, N'Equinox')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (18, N'Rogue')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (19, N'Sorento')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (20, N'Model S')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (21, N'F-150')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (22, N'Silverado')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (23, N'Tacoma')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (24, N'Wrangler')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (25, N'Sierra')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (26, N'Charger')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (27, N'Impreza')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (28, N'Golf')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (29, N'5 Series')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (30, N'E-Class')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (31, N'Elantra')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (32, N'Escape')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (33, N'Civic')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (34, N'Corolla')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (35, N'Impala')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (36, N'Maxima')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (37, N'Optima')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (38, N'Q5')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (39, N'XC90')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (40, N'F-Type')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (41, N'911')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (42, N'RX')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (43, N'Escalade')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (44, N'Q50')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (45, N'Range Rover')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (46, N'Model 3')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (47, N'Tucson')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (48, N'Forester')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (49, N'Mazda3')
INSERT [dbo].[Modelo] ([modelo_id], [modelo_tipo]) VALUES (50, N'Edge')
SET IDENTITY_INSERT [dbo].[Modelo] OFF
GO
SET IDENTITY_INSERT [dbo].[Perfil] ON 

INSERT [dbo].[Perfil] ([perfil_id], [perfil_nombreRol], [perfil_estado]) VALUES (1, N'administrador', 1)
INSERT [dbo].[Perfil] ([perfil_id], [perfil_nombreRol], [perfil_estado]) VALUES (2, N'cliente', 1)
INSERT [dbo].[Perfil] ([perfil_id], [perfil_nombreRol], [perfil_estado]) VALUES (3, N'empleado', 1)
SET IDENTITY_INSERT [dbo].[Perfil] OFF
GO
SET IDENTITY_INSERT [dbo].[Proveedor] ON 

INSERT [dbo].[Proveedor] ([proveedor_id], [proveedor_direccion], [proveedor_provincia], [proveedor_pais], [proveedor_nombreEmpresa]) VALUES (1, N'El Bosque N._San Luis', N'Ecuador', N'Quito', N'LosPisAgilesEmpresa')
SET IDENTITY_INSERT [dbo].[Proveedor] OFF
GO
SET IDENTITY_INSERT [dbo].[PruebaManejo] ON 

INSERT [dbo].[PruebaManejo] ([pruebaManejo_id], [pruebaManejo_estado], [pruebaManejo_descripcion], [pruebaManejo_nivelSatisfaccion]) VALUES (1, 1, N'probando automovil', 3)
INSERT [dbo].[PruebaManejo] ([pruebaManejo_id], [pruebaManejo_estado], [pruebaManejo_descripcion], [pruebaManejo_nivelSatisfaccion]) VALUES (2, 1, N'los frenos fallaron un poco en la prueba', 1)
INSERT [dbo].[PruebaManejo] ([pruebaManejo_id], [pruebaManejo_estado], [pruebaManejo_descripcion], [pruebaManejo_nivelSatisfaccion]) VALUES (3, 1, N'el vidrio estaba muy lastimado', 0)
INSERT [dbo].[PruebaManejo] ([pruebaManejo_id], [pruebaManejo_estado], [pruebaManejo_descripcion], [pruebaManejo_nivelSatisfaccion]) VALUES (4, 1, N'execlente automovil', 3)
INSERT [dbo].[PruebaManejo] ([pruebaManejo_id], [pruebaManejo_estado], [pruebaManejo_descripcion], [pruebaManejo_nivelSatisfaccion]) VALUES (5, 1, N'me fascino demasiado la prueba', 3)
INSERT [dbo].[PruebaManejo] ([pruebaManejo_id], [pruebaManejo_estado], [pruebaManejo_descripcion], [pruebaManejo_nivelSatisfaccion]) VALUES (6, 1, N'esta prueba es increible', 3)
INSERT [dbo].[PruebaManejo] ([pruebaManejo_id], [pruebaManejo_estado], [pruebaManejo_descripcion], [pruebaManejo_nivelSatisfaccion]) VALUES (7, 1, N'este es una prueba de manejo de ejemplo, borrar esta prueba de manejo', 0)
SET IDENTITY_INSERT [dbo].[PruebaManejo] OFF
GO
SET IDENTITY_INSERT [dbo].[Reserva] ON 

INSERT [dbo].[Reserva] ([reserva_id], [reserva_compraId], [reserva_pruebaManejoId], [reserva_usuarioPerfilId], [reserva_estado], [reserva_AutoId], [reserva_asunto], [reserva_fecha], [reserva_hora]) VALUES (1, NULL, 1, 31, 1, 1, N'aqui tu asunto', CAST(N'0001-01-01T00:00:00.0000000' AS DateTime2), N'10:00 AM')
INSERT [dbo].[Reserva] ([reserva_id], [reserva_compraId], [reserva_pruebaManejoId], [reserva_usuarioPerfilId], [reserva_estado], [reserva_AutoId], [reserva_asunto], [reserva_fecha], [reserva_hora]) VALUES (2, NULL, 2, 33, 2, 2, N'aqui tu asunto', CAST(N'0001-01-01T00:00:00.0000000' AS DateTime2), N'10:00 AM')
INSERT [dbo].[Reserva] ([reserva_id], [reserva_compraId], [reserva_pruebaManejoId], [reserva_usuarioPerfilId], [reserva_estado], [reserva_AutoId], [reserva_asunto], [reserva_fecha], [reserva_hora]) VALUES (3, NULL, 3, 35, 3, 3, N'aqui tu asunto', CAST(N'0001-01-01T00:00:00.0000000' AS DateTime2), N'10:00 AM')
INSERT [dbo].[Reserva] ([reserva_id], [reserva_compraId], [reserva_pruebaManejoId], [reserva_usuarioPerfilId], [reserva_estado], [reserva_AutoId], [reserva_asunto], [reserva_fecha], [reserva_hora]) VALUES (4, 1, NULL, 31, 4, 1, N'aqui tu asunto', CAST(N'0001-01-01T00:00:00.0000000' AS DateTime2), N'10:00 AM')
INSERT [dbo].[Reserva] ([reserva_id], [reserva_compraId], [reserva_pruebaManejoId], [reserva_usuarioPerfilId], [reserva_estado], [reserva_AutoId], [reserva_asunto], [reserva_fecha], [reserva_hora]) VALUES (5, 2, NULL, 33, 5, 2, N'aqui tu asunto', CAST(N'0001-01-01T00:00:00.0000000' AS DateTime2), N'10:00 AM')
INSERT [dbo].[Reserva] ([reserva_id], [reserva_compraId], [reserva_pruebaManejoId], [reserva_usuarioPerfilId], [reserva_estado], [reserva_AutoId], [reserva_asunto], [reserva_fecha], [reserva_hora]) VALUES (6, 3, NULL, 35, 6, 3, N'aqui tu asunto', CAST(N'0001-01-01T00:00:00.0000000' AS DateTime2), N'10:00 AM')
INSERT [dbo].[Reserva] ([reserva_id], [reserva_compraId], [reserva_pruebaManejoId], [reserva_usuarioPerfilId], [reserva_estado], [reserva_AutoId], [reserva_asunto], [reserva_fecha], [reserva_hora]) VALUES (8, 8, 5, 55, 8, 11, N'aqui tu asunto', CAST(N'0001-01-01T00:00:00.0000000' AS DateTime2), N'10:00 AM')
INSERT [dbo].[Reserva] ([reserva_id], [reserva_compraId], [reserva_pruebaManejoId], [reserva_usuarioPerfilId], [reserva_estado], [reserva_AutoId], [reserva_asunto], [reserva_fecha], [reserva_hora]) VALUES (9, 9, 6, 53, 9, 12, N'aqui tu asunto', CAST(N'0001-01-01T00:00:00.0000000' AS DateTime2), N'10:00 AM')
INSERT [dbo].[Reserva] ([reserva_id], [reserva_compraId], [reserva_pruebaManejoId], [reserva_usuarioPerfilId], [reserva_estado], [reserva_AutoId], [reserva_asunto], [reserva_fecha], [reserva_hora]) VALUES (11, 9, 7, 31, 2, 94, N'aqui tu asunto', CAST(N'0001-01-01T00:00:00.0000000' AS DateTime2), N'10:00 AM')
SET IDENTITY_INSERT [dbo].[Reserva] OFF
GO
SET IDENTITY_INSERT [dbo].[Usuario] ON 

INSERT [dbo].[Usuario] ([usuario_id], [usuario_nombre], [usuario_apellido], [usuario_cedula], [usuario_celular], [usuario_correo], [usuario_contrasena]) VALUES (1, N'Juan', N'Gómez', N'1234567890', N'0987654321', N'juan_gomez@example.com', N'A289424F-2958-4')
INSERT [dbo].[Usuario] ([usuario_id], [usuario_nombre], [usuario_apellido], [usuario_cedula], [usuario_celular], [usuario_correo], [usuario_contrasena]) VALUES (2, N'Ana', N'Martínez', N'0987654321', N'1234567890', N'ana_martinez@example.com', N'C7DA1366-59BC-4')
INSERT [dbo].[Usuario] ([usuario_id], [usuario_nombre], [usuario_apellido], [usuario_cedula], [usuario_celular], [usuario_correo], [usuario_contrasena]) VALUES (3, N'Pedro', N'López', N'9876543210', N'0123456789', N'pedro_lopez@example.com', N'AF96378E-A522-4')
INSERT [dbo].[Usuario] ([usuario_id], [usuario_nombre], [usuario_apellido], [usuario_cedula], [usuario_celular], [usuario_correo], [usuario_contrasena]) VALUES (4, N'Luisa', N'Sánchez', N'0123456789', N'9876543210', N'luisa_sanchez@example.com', N'36387DED-44B5-4')
INSERT [dbo].[Usuario] ([usuario_id], [usuario_nombre], [usuario_apellido], [usuario_cedula], [usuario_celular], [usuario_correo], [usuario_contrasena]) VALUES (5, N'Carlos', N'Fernández', N'2345678901', N'8765432109', N'carlos_fernandez@example.com', N'347E0A31-6F93-4')
INSERT [dbo].[Usuario] ([usuario_id], [usuario_nombre], [usuario_apellido], [usuario_cedula], [usuario_celular], [usuario_correo], [usuario_contrasena]) VALUES (6, N'Laura', N'González', N'3456789012', N'7654321098', N'laura_gonzalez@example.com', N'96061242-8C78-4')
INSERT [dbo].[Usuario] ([usuario_id], [usuario_nombre], [usuario_apellido], [usuario_cedula], [usuario_celular], [usuario_correo], [usuario_contrasena]) VALUES (7, N'Miguel', N'Díaz', N'4567890123', N'6543210987', N'miguel_diaz@example.com', N'765FFBB7-3630-4')
INSERT [dbo].[Usuario] ([usuario_id], [usuario_nombre], [usuario_apellido], [usuario_cedula], [usuario_celular], [usuario_correo], [usuario_contrasena]) VALUES (8, N'Carmen', N'Hernández', N'5678901234', N'5432109876', N'carmen_hernandez@example.com', N'B55E14E1-8FE6-4')
INSERT [dbo].[Usuario] ([usuario_id], [usuario_nombre], [usuario_apellido], [usuario_cedula], [usuario_celular], [usuario_correo], [usuario_contrasena]) VALUES (9, N'Pablo', N'Martínez', N'6789012345', N'4321098765', N'pablo_martinez@example.com', N'74D82C7E-FE83-4')
INSERT [dbo].[Usuario] ([usuario_id], [usuario_nombre], [usuario_apellido], [usuario_cedula], [usuario_celular], [usuario_correo], [usuario_contrasena]) VALUES (10, N'Sofía', N'Pérez', N'7890123456', N'3210987654', N'sofia_perez@example.com', N'5320D3B4-5D25-4')
INSERT [dbo].[Usuario] ([usuario_id], [usuario_nombre], [usuario_apellido], [usuario_cedula], [usuario_celular], [usuario_correo], [usuario_contrasena]) VALUES (11, N'Javier', N'Rodríguez', N'8901234567', N'2109876543', N'javier_rodriguez@example.com', N'8E6AAA71-DD6E-4')
INSERT [dbo].[Usuario] ([usuario_id], [usuario_nombre], [usuario_apellido], [usuario_cedula], [usuario_celular], [usuario_correo], [usuario_contrasena]) VALUES (12, N'Valeria', N'Gómez', N'9012345678', N'1098765432', N'valeria_gomez@example.com', N'D2D61F62-45F7-4')
INSERT [dbo].[Usuario] ([usuario_id], [usuario_nombre], [usuario_apellido], [usuario_cedula], [usuario_celular], [usuario_correo], [usuario_contrasena]) VALUES (13, N'Andrés', N'López', N'1123456789', N'9876543210', N'andres_lopez@example.com', N'5FC5EA85-49C5-4')
INSERT [dbo].[Usuario] ([usuario_id], [usuario_nombre], [usuario_apellido], [usuario_cedula], [usuario_celular], [usuario_correo], [usuario_contrasena]) VALUES (14, N'Isabel', N'Fernández', N'2234567890', N'8765432109', N'isabel_fernandez@example.com', N'8CCB26D7-9AC5-4')
INSERT [dbo].[Usuario] ([usuario_id], [usuario_nombre], [usuario_apellido], [usuario_cedula], [usuario_celular], [usuario_correo], [usuario_contrasena]) VALUES (15, N'Martín', N'Sánchez', N'3345678901', N'7654321098', N'martin_sanchez@example.com', N'7117E35A-FE1E-4')
INSERT [dbo].[Usuario] ([usuario_id], [usuario_nombre], [usuario_apellido], [usuario_cedula], [usuario_celular], [usuario_correo], [usuario_contrasena]) VALUES (16, N'Lucía', N'Díaz', N'4456789012', N'6543210987', N'lucia_diaz@example.com', N'989053D3-4012-4')
INSERT [dbo].[Usuario] ([usuario_id], [usuario_nombre], [usuario_apellido], [usuario_cedula], [usuario_celular], [usuario_correo], [usuario_contrasena]) VALUES (17, N'Alejandro', N'Hernández', N'5567890123', N'5432109876', N'alejandro_hernandez@example.com', N'97C0895B-3A65-4')
INSERT [dbo].[Usuario] ([usuario_id], [usuario_nombre], [usuario_apellido], [usuario_cedula], [usuario_celular], [usuario_correo], [usuario_contrasena]) VALUES (18, N'María', N'Martínez', N'6678901234', N'4321098765', N'maria_martinez@example.com', N'A0930A73-EF33-4')
INSERT [dbo].[Usuario] ([usuario_id], [usuario_nombre], [usuario_apellido], [usuario_cedula], [usuario_celular], [usuario_correo], [usuario_contrasena]) VALUES (19, N'Raúl', N'Pérez', N'7789012345', N'3210987654', N'raul_perez@example.com', N'AD434FAB-9CC8-4')
INSERT [dbo].[Usuario] ([usuario_id], [usuario_nombre], [usuario_apellido], [usuario_cedula], [usuario_celular], [usuario_correo], [usuario_contrasena]) VALUES (20, N'Elena', N'Rodríguez', N'8890123456', N'2109876543', N'elena_rodriguez@example.com', N'4D0046BF-88DF-4')
INSERT [dbo].[Usuario] ([usuario_id], [usuario_nombre], [usuario_apellido], [usuario_cedula], [usuario_celular], [usuario_correo], [usuario_contrasena]) VALUES (21, N'Hugo', N'Gómez', N'9901234567', N'1098765432', N'hugo_gomez@example.com', N'5A55E369-8513-4')
INSERT [dbo].[Usuario] ([usuario_id], [usuario_nombre], [usuario_apellido], [usuario_cedula], [usuario_celular], [usuario_correo], [usuario_contrasena]) VALUES (22, N'Adriana', N'López', N'0012345678', N'9876543210', N'adriana_lopez@example.com', N'00757D72-F916-4')
INSERT [dbo].[Usuario] ([usuario_id], [usuario_nombre], [usuario_apellido], [usuario_cedula], [usuario_celular], [usuario_correo], [usuario_contrasena]) VALUES (23, N'Fernando', N'Fernández', N'1123456789', N'8765432109', N'fernando_fernandez@example.com', N'2CC0625C-B6DA-4')
INSERT [dbo].[Usuario] ([usuario_id], [usuario_nombre], [usuario_apellido], [usuario_cedula], [usuario_celular], [usuario_correo], [usuario_contrasena]) VALUES (24, N'Natalia', N'Sánchez', N'2234567890', N'7654321098', N'natalia_sanchez@example.com', N'BCBD8029-2E2E-4')
INSERT [dbo].[Usuario] ([usuario_id], [usuario_nombre], [usuario_apellido], [usuario_cedula], [usuario_celular], [usuario_correo], [usuario_contrasena]) VALUES (25, N'Diego', N'Díaz', N'3345678901', N'6543210987', N'diego_diaz@example.com', N'D4BD9B50-928A-4')
INSERT [dbo].[Usuario] ([usuario_id], [usuario_nombre], [usuario_apellido], [usuario_cedula], [usuario_celular], [usuario_correo], [usuario_contrasena]) VALUES (26, N'Sara', N'Hernández', N'4456789012', N'5432109876', N'sara_hernandez@example.com', N'6834F30C-CC6C-4')
INSERT [dbo].[Usuario] ([usuario_id], [usuario_nombre], [usuario_apellido], [usuario_cedula], [usuario_celular], [usuario_correo], [usuario_contrasena]) VALUES (27, N'Roberto', N'Martínez', N'5567890123', N'4321098765', N'roberto_martinez@example.com', N'997F80FB-169E-4')
INSERT [dbo].[Usuario] ([usuario_id], [usuario_nombre], [usuario_apellido], [usuario_cedula], [usuario_celular], [usuario_correo], [usuario_contrasena]) VALUES (30, N'esthe', N'phoni', N'1023489098', N'2222222222', N'esthe@hotmail.com', N'JFIOD9-JJJ90')
INSERT [dbo].[Usuario] ([usuario_id], [usuario_nombre], [usuario_apellido], [usuario_cedula], [usuario_celular], [usuario_correo], [usuario_contrasena]) VALUES (36, N'ejemplo', N'ejemplo', N'0923454345', N'1234234956', N'ejemplo@hotmail.com', N'DJJJi-FJI290')
INSERT [dbo].[Usuario] ([usuario_id], [usuario_nombre], [usuario_apellido], [usuario_cedula], [usuario_celular], [usuario_correo], [usuario_contrasena]) VALUES (66, N'mic', N'mic', N'0956898777', N'1112229087', N'mic@hotmail.com', N'A909JJJJI-4')
INSERT [dbo].[Usuario] ([usuario_id], [usuario_nombre], [usuario_apellido], [usuario_cedula], [usuario_celular], [usuario_correo], [usuario_contrasena]) VALUES (67, N'michael', N'ortega', N'1232343232', N'0956789878', N'michael@hotmail.com', N'JIJFIJ909-9')
SET IDENTITY_INSERT [dbo].[Usuario] OFF
GO
SET IDENTITY_INSERT [dbo].[UsuarioPerfil] ON 

INSERT [dbo].[UsuarioPerfil] ([usuarioPerfil_id], [usuarioPerfil_usuarioId], [usuarioPerfil_perfilId]) VALUES (30, 1, 1)
INSERT [dbo].[UsuarioPerfil] ([usuarioPerfil_id], [usuarioPerfil_usuarioId], [usuarioPerfil_perfilId]) VALUES (31, 2, 2)
INSERT [dbo].[UsuarioPerfil] ([usuarioPerfil_id], [usuarioPerfil_usuarioId], [usuarioPerfil_perfilId]) VALUES (32, 3, 3)
INSERT [dbo].[UsuarioPerfil] ([usuarioPerfil_id], [usuarioPerfil_usuarioId], [usuarioPerfil_perfilId]) VALUES (33, 4, 2)
INSERT [dbo].[UsuarioPerfil] ([usuarioPerfil_id], [usuarioPerfil_usuarioId], [usuarioPerfil_perfilId]) VALUES (34, 5, 1)
INSERT [dbo].[UsuarioPerfil] ([usuarioPerfil_id], [usuarioPerfil_usuarioId], [usuarioPerfil_perfilId]) VALUES (35, 6, 2)
INSERT [dbo].[UsuarioPerfil] ([usuarioPerfil_id], [usuarioPerfil_usuarioId], [usuarioPerfil_perfilId]) VALUES (36, 7, 1)
INSERT [dbo].[UsuarioPerfil] ([usuarioPerfil_id], [usuarioPerfil_usuarioId], [usuarioPerfil_perfilId]) VALUES (37, 8, 2)
INSERT [dbo].[UsuarioPerfil] ([usuarioPerfil_id], [usuarioPerfil_usuarioId], [usuarioPerfil_perfilId]) VALUES (38, 9, 3)
INSERT [dbo].[UsuarioPerfil] ([usuarioPerfil_id], [usuarioPerfil_usuarioId], [usuarioPerfil_perfilId]) VALUES (39, 10, 2)
INSERT [dbo].[UsuarioPerfil] ([usuarioPerfil_id], [usuarioPerfil_usuarioId], [usuarioPerfil_perfilId]) VALUES (40, 11, 1)
INSERT [dbo].[UsuarioPerfil] ([usuarioPerfil_id], [usuarioPerfil_usuarioId], [usuarioPerfil_perfilId]) VALUES (41, 12, 2)
INSERT [dbo].[UsuarioPerfil] ([usuarioPerfil_id], [usuarioPerfil_usuarioId], [usuarioPerfil_perfilId]) VALUES (42, 13, 1)
INSERT [dbo].[UsuarioPerfil] ([usuarioPerfil_id], [usuarioPerfil_usuarioId], [usuarioPerfil_perfilId]) VALUES (43, 14, 2)
INSERT [dbo].[UsuarioPerfil] ([usuarioPerfil_id], [usuarioPerfil_usuarioId], [usuarioPerfil_perfilId]) VALUES (44, 15, 3)
INSERT [dbo].[UsuarioPerfil] ([usuarioPerfil_id], [usuarioPerfil_usuarioId], [usuarioPerfil_perfilId]) VALUES (45, 16, 2)
INSERT [dbo].[UsuarioPerfil] ([usuarioPerfil_id], [usuarioPerfil_usuarioId], [usuarioPerfil_perfilId]) VALUES (46, 17, 1)
INSERT [dbo].[UsuarioPerfil] ([usuarioPerfil_id], [usuarioPerfil_usuarioId], [usuarioPerfil_perfilId]) VALUES (47, 18, 2)
INSERT [dbo].[UsuarioPerfil] ([usuarioPerfil_id], [usuarioPerfil_usuarioId], [usuarioPerfil_perfilId]) VALUES (48, 19, 1)
INSERT [dbo].[UsuarioPerfil] ([usuarioPerfil_id], [usuarioPerfil_usuarioId], [usuarioPerfil_perfilId]) VALUES (49, 20, 2)
INSERT [dbo].[UsuarioPerfil] ([usuarioPerfil_id], [usuarioPerfil_usuarioId], [usuarioPerfil_perfilId]) VALUES (50, 21, 3)
INSERT [dbo].[UsuarioPerfil] ([usuarioPerfil_id], [usuarioPerfil_usuarioId], [usuarioPerfil_perfilId]) VALUES (51, 22, 2)
INSERT [dbo].[UsuarioPerfil] ([usuarioPerfil_id], [usuarioPerfil_usuarioId], [usuarioPerfil_perfilId]) VALUES (52, 23, 1)
INSERT [dbo].[UsuarioPerfil] ([usuarioPerfil_id], [usuarioPerfil_usuarioId], [usuarioPerfil_perfilId]) VALUES (53, 24, 2)
INSERT [dbo].[UsuarioPerfil] ([usuarioPerfil_id], [usuarioPerfil_usuarioId], [usuarioPerfil_perfilId]) VALUES (54, 25, 1)
INSERT [dbo].[UsuarioPerfil] ([usuarioPerfil_id], [usuarioPerfil_usuarioId], [usuarioPerfil_perfilId]) VALUES (55, 26, 2)
INSERT [dbo].[UsuarioPerfil] ([usuarioPerfil_id], [usuarioPerfil_usuarioId], [usuarioPerfil_perfilId]) VALUES (56, 27, 3)
SET IDENTITY_INSERT [dbo].[UsuarioPerfil] OFF
GO
/****** Object:  Index [IX_Auto_auto_inventarioId]    Script Date: 2/2/2024 12:31:12 AM ******/
CREATE NONCLUSTERED INDEX [IX_Auto_auto_inventarioId] ON [dbo].[Auto]
(
	[auto_inventarioId] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [IX_Compra_compra_metodoPagoId]    Script Date: 2/2/2024 12:31:12 AM ******/
CREATE NONCLUSTERED INDEX [IX_Compra_compra_metodoPagoId] ON [dbo].[Compra]
(
	[compra_metodoPagoId] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [IX_Inventario_inventario_carroceriaId]    Script Date: 2/2/2024 12:31:12 AM ******/
CREATE NONCLUSTERED INDEX [IX_Inventario_inventario_carroceriaId] ON [dbo].[Inventario]
(
	[inventario_carroceriaId] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [IX_Inventario_inventario_colorId]    Script Date: 2/2/2024 12:31:12 AM ******/
CREATE NONCLUSTERED INDEX [IX_Inventario_inventario_colorId] ON [dbo].[Inventario]
(
	[inventario_colorId] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [IX_Inventario_inventario_marcaId]    Script Date: 2/2/2024 12:31:12 AM ******/
CREATE NONCLUSTERED INDEX [IX_Inventario_inventario_marcaId] ON [dbo].[Inventario]
(
	[inventario_marcaId] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [IX_Inventario_inventario_modeloId]    Script Date: 2/2/2024 12:31:12 AM ******/
CREATE NONCLUSTERED INDEX [IX_Inventario_inventario_modeloId] ON [dbo].[Inventario]
(
	[inventario_modeloId] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [IX_Marca_marca_proveedorId]    Script Date: 2/2/2024 12:31:12 AM ******/
CREATE NONCLUSTERED INDEX [IX_Marca_marca_proveedorId] ON [dbo].[Marca]
(
	[marca_proveedorId] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [IX_Reserva_reserva_AutoId]    Script Date: 2/2/2024 12:31:12 AM ******/
CREATE NONCLUSTERED INDEX [IX_Reserva_reserva_AutoId] ON [dbo].[Reserva]
(
	[reserva_AutoId] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [IX_Reserva_reserva_compraId]    Script Date: 2/2/2024 12:31:12 AM ******/
CREATE NONCLUSTERED INDEX [IX_Reserva_reserva_compraId] ON [dbo].[Reserva]
(
	[reserva_compraId] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [IX_Reserva_reserva_pruebaManejoId]    Script Date: 2/2/2024 12:31:12 AM ******/
CREATE NONCLUSTERED INDEX [IX_Reserva_reserva_pruebaManejoId] ON [dbo].[Reserva]
(
	[reserva_pruebaManejoId] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [IX_Reserva_reserva_usuarioPerfilId]    Script Date: 2/2/2024 12:31:12 AM ******/
CREATE NONCLUSTERED INDEX [IX_Reserva_reserva_usuarioPerfilId] ON [dbo].[Reserva]
(
	[reserva_usuarioPerfilId] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [IX_UsuarioPerfil_usuarioPerfil_perfilId]    Script Date: 2/2/2024 12:31:12 AM ******/
CREATE NONCLUSTERED INDEX [IX_UsuarioPerfil_usuarioPerfil_perfilId] ON [dbo].[UsuarioPerfil]
(
	[usuarioPerfil_perfilId] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
/****** Object:  Index [IX_UsuarioPerfil_usuarioPerfil_usuarioId]    Script Date: 2/2/2024 12:31:12 AM ******/
CREATE NONCLUSTERED INDEX [IX_UsuarioPerfil_usuarioPerfil_usuarioId] ON [dbo].[UsuarioPerfil]
(
	[usuarioPerfil_usuarioId] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON, OPTIMIZE_FOR_SEQUENTIAL_KEY = OFF) ON [PRIMARY]
GO
ALTER TABLE [dbo].[Reserva] ADD  DEFAULT (N'') FOR [reserva_asunto]
GO
ALTER TABLE [dbo].[Reserva] ADD  DEFAULT ('0001-01-01T00:00:00.0000000') FOR [reserva_fecha]
GO
ALTER TABLE [dbo].[Reserva] ADD  DEFAULT (N'') FOR [reserva_hora]
GO
ALTER TABLE [dbo].[Auto]  WITH CHECK ADD  CONSTRAINT [FK_Auto_Inventario_auto_inventarioId] FOREIGN KEY([auto_inventarioId])
REFERENCES [dbo].[Inventario] ([inventario_id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[Auto] CHECK CONSTRAINT [FK_Auto_Inventario_auto_inventarioId]
GO
ALTER TABLE [dbo].[Compra]  WITH CHECK ADD  CONSTRAINT [FK_Compra_MetodoPago_compra_metodoPagoId] FOREIGN KEY([compra_metodoPagoId])
REFERENCES [dbo].[MetodoPago] ([metodoPago_id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[Compra] CHECK CONSTRAINT [FK_Compra_MetodoPago_compra_metodoPagoId]
GO
ALTER TABLE [dbo].[Inventario]  WITH CHECK ADD  CONSTRAINT [FK_Inventario_Carroceria_inventario_carroceriaId] FOREIGN KEY([inventario_carroceriaId])
REFERENCES [dbo].[Carroceria] ([carroceria_id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[Inventario] CHECK CONSTRAINT [FK_Inventario_Carroceria_inventario_carroceriaId]
GO
ALTER TABLE [dbo].[Inventario]  WITH CHECK ADD  CONSTRAINT [FK_Inventario_Color_inventario_colorId] FOREIGN KEY([inventario_colorId])
REFERENCES [dbo].[Color] ([color_id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[Inventario] CHECK CONSTRAINT [FK_Inventario_Color_inventario_colorId]
GO
ALTER TABLE [dbo].[Inventario]  WITH CHECK ADD  CONSTRAINT [FK_Inventario_Marca_inventario_marcaId] FOREIGN KEY([inventario_marcaId])
REFERENCES [dbo].[Marca] ([marca_id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[Inventario] CHECK CONSTRAINT [FK_Inventario_Marca_inventario_marcaId]
GO
ALTER TABLE [dbo].[Inventario]  WITH CHECK ADD  CONSTRAINT [FK_Inventario_Modelo_inventario_modeloId] FOREIGN KEY([inventario_modeloId])
REFERENCES [dbo].[Modelo] ([modelo_id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[Inventario] CHECK CONSTRAINT [FK_Inventario_Modelo_inventario_modeloId]
GO
ALTER TABLE [dbo].[Marca]  WITH CHECK ADD  CONSTRAINT [FK_Marca_Proveedor_marca_proveedorId] FOREIGN KEY([marca_proveedorId])
REFERENCES [dbo].[Proveedor] ([proveedor_id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[Marca] CHECK CONSTRAINT [FK_Marca_Proveedor_marca_proveedorId]
GO
ALTER TABLE [dbo].[Reserva]  WITH CHECK ADD  CONSTRAINT [FK_Reserva_Auto_reserva_AutoId] FOREIGN KEY([reserva_AutoId])
REFERENCES [dbo].[Auto] ([auto_id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[Reserva] CHECK CONSTRAINT [FK_Reserva_Auto_reserva_AutoId]
GO
ALTER TABLE [dbo].[Reserva]  WITH CHECK ADD  CONSTRAINT [FK_Reserva_Compra_reserva_compraId] FOREIGN KEY([reserva_compraId])
REFERENCES [dbo].[Compra] ([compra_id])
GO
ALTER TABLE [dbo].[Reserva] CHECK CONSTRAINT [FK_Reserva_Compra_reserva_compraId]
GO
ALTER TABLE [dbo].[Reserva]  WITH CHECK ADD  CONSTRAINT [FK_Reserva_PruebaManejo_reserva_pruebaManejoId] FOREIGN KEY([reserva_pruebaManejoId])
REFERENCES [dbo].[PruebaManejo] ([pruebaManejo_id])
GO
ALTER TABLE [dbo].[Reserva] CHECK CONSTRAINT [FK_Reserva_PruebaManejo_reserva_pruebaManejoId]
GO
ALTER TABLE [dbo].[Reserva]  WITH CHECK ADD  CONSTRAINT [FK_Reserva_UsuarioPerfil_reserva_usuarioPerfilId] FOREIGN KEY([reserva_usuarioPerfilId])
REFERENCES [dbo].[UsuarioPerfil] ([usuarioPerfil_id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[Reserva] CHECK CONSTRAINT [FK_Reserva_UsuarioPerfil_reserva_usuarioPerfilId]
GO
ALTER TABLE [dbo].[UsuarioPerfil]  WITH CHECK ADD  CONSTRAINT [FK_UsuarioPerfil_Perfil_usuarioPerfil_perfilId] FOREIGN KEY([usuarioPerfil_perfilId])
REFERENCES [dbo].[Perfil] ([perfil_id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[UsuarioPerfil] CHECK CONSTRAINT [FK_UsuarioPerfil_Perfil_usuarioPerfil_perfilId]
GO
ALTER TABLE [dbo].[UsuarioPerfil]  WITH CHECK ADD  CONSTRAINT [FK_UsuarioPerfil_Usuario_usuarioPerfil_usuarioId] FOREIGN KEY([usuarioPerfil_usuarioId])
REFERENCES [dbo].[Usuario] ([usuario_id])
ON DELETE CASCADE
GO
ALTER TABLE [dbo].[UsuarioPerfil] CHECK CONSTRAINT [FK_UsuarioPerfil_Usuario_usuarioPerfil_usuarioId]
GO
/****** Object:  StoredProcedure [dbo].[ActualizarPTabla]    Script Date: 2/2/2024 12:31:12 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[ActualizarPTabla]
	@TablaEntrada NVARCHAR(MAX),
	@NRelaciones INT,
	@Campos NVARCHAR(MAX),
	@Valor NVARCHAR(MAX),
	@NCondicional INT = 0, 
	@CampoCondicional NVARCHAR(MAX) = '',
	@ValorCondicional NVARCHAR(MAX) = ''
AS
BEGIN 
	BEGIN TRY
		DECLARE @resultadoTexto NVARCHAR(MAX) = '';
		EXECUTE ListarPTabla @Tabla = @TablaEntrada, @nRelaciones = @NCondicional, @campo=@CampoCondicional, @valor=@ValorCondicional, @resultadoSalida = @resultadoTexto OUTPUT;

		IF (NOT @TablaEntrada = '') AND (NOT @CampoCondicional = '') AND (NOT @ValorCondicional = '') AND @resultadoTexto != 'sin_coincidencia' AND @resultadoTexto != 'error' BEGIN 
			IF @NRelaciones > 0 AND (NOT @Campos = '') AND (NOT @Valor = '')  BEGIN 
				DECLARE @CMD NVARCHAR(MAX);
				SET @CMD = N'UPDATE ' + @TablaEntrada + ' SET ';

				
				DECLARE @Relacion NVARCHAR(MAX) = dbo.FuncionString(@NRelaciones, @Campos, @Valor, 'actualizar');
				SET @CMD += @Relacion;

				-- creamos la clausula where
				SET @CMD += ' WHERE ';
				DECLARE @Condicional NVARCHAR(MAX) = dbo.FuncionString( @NCondicional, @CampoCondicional, @ValorCondicional, 'actualizar2');
				SET @CMD += @Condicional;
		
				EXECUTE(@CMD);
				
				SELECT 'ok';
			END 
			ELSE BEGIN 
				SELECT 'error';
			END
		END
		ELSE BEGIN 
			SELECT @resultadoTexto;
		END
	END TRY
	BEGIN CATCH
		SELECT 'error', ERROR_MESSAGE();
	END CATCH
END
GO
/****** Object:  StoredProcedure [dbo].[CrearPTabla]    Script Date: 2/2/2024 12:31:12 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[CrearPTabla]
	@TablaEntrada NVARCHAR(90),
	@NumeroCampos INT,
	@Esquema NVARCHAR(MAX),
	@Registros NVARCHAR(MAX)
AS
BEGIN
	BEGIN TRY	
		DECLARE @SQL NVARCHAR(MAX) = N'INSERT INTO ' + @TablaEntrada + '(' + @Esquema + ')' + ' VALUES(';
		DECLARE @VALUES NVARCHAR(MAX) = dbo.FuncionString(@NumeroCampos, @Esquema, @Registros, 'crear'); 
		SET @SQL += @VALUES + ')';
		
		EXECUTE( @SQL );
		
		DECLARE @resultadoTexto NVARCHAR(MAX) = '';
		EXECUTE ListarPTabla @Tabla = @TablaEntrada, @nRelaciones = @NumeroCampos, @campo=@Esquema, @valor=@Registros, @resultadoSalida = @resultadoTexto OUTPUT;

		IF @resultadoTexto != 'error' AND @resultadoTexto != 'sin_coincidencia' BEGIN 
			SELECT 'ok';
		END 
		ELSE BEGIN 
			SELECT 'error';
		END 

	END TRY
	BEGIN CATCH
		SELECT 'error', ERROR_MESSAGE();
	END CATCH
END
GO
/****** Object:  StoredProcedure [dbo].[EliminarPTabla]    Script Date: 2/2/2024 12:31:12 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[EliminarPTabla]
	@TablaEntrada NVARCHAR(MAX) = '',
	@key NVARCHAR(MAX) = '',
	@value NVARCHAR(MAX) = ''
AS
BEGIN
	BEGIN TRY 
		DECLARE @resultadoTexto NVARCHAR(MAX) = '';
		EXECUTE ListarPTabla @Tabla = @TablaEntrada, @nRelaciones = 1, @campo=@key, @valor=@value, @resultadoSalida = @resultadoTexto OUTPUT;

		IF (NOT @TablaEntrada = '') AND (NOT @key = '') AND (NOT @value = '') AND @resultadoTexto != 'sin_coincidencia' AND @resultadoTexto != 'error' BEGIN
			DECLARE @CMD NVARCHAR(MAX) = 'DELETE FROM';
			SET @CMD += ' ' + @TablaEntrada;
			SET @CMD += ' ' + 'WHERE';
			SET @CMD += ' ' + @key;
			SET @CMD += ' = ' + @value;

			EXECUTE(@CMD);
			SELECT 'ok';
		END
		ELSE BEGIN 
			SELECT @resultadoTexto;
		END
	END TRY 
	BEGIN CATCH 
		SELECT 'error';
	END CATCH 
END
GO
/****** Object:  StoredProcedure [dbo].[ListarPTabla]    Script Date: 2/2/2024 12:31:12 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[ListarPTabla]
	@Tabla NVARCHAR(MAX),
	@nRelaciones INT = 0,
	@campo NVARCHAR(MAX) = '',
	@valor NVARCHAR(MAX) = '',
	@resultadoSalida NVARCHAR(MAX) = '' OUTPUT
AS
BEGIN
	BEGIN TRY		
		CREATE TABLE #resultado( resultado_id INT );

		-- con este verifico cuantas claves FK tiene una tabla en general
		DECLARE @NumeroLlaves INT;
		SELECT @NumeroLlaves = COUNT(COLUMN_NAME) 
		FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE
		WHERE TABLE_NAME = @Tabla;

		-- obtenmos minimo y maximo en ordenal_position de nuestra tabla de entrada.
		DECLARE @minimo INT = ( SELECT TOP 1 c.ORDINAL_POSITION
		FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE as k 
			INNER JOIN INFORMATION_SCHEMA.COLUMNS as c 
				ON k.COLUMN_NAME = c.COLUMN_NAME
		WHERE k.TABLE_NAME = @Tabla AND k.CONSTRAINT_NAME LIKE '%FK%'
		ORDER BY c.ORDINAL_POSITION ASC );

		DECLARE @maximo INT = (SELECT TOP 1 c.ORDINAL_POSITION
		FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE as k 
			INNER JOIN INFORMATION_SCHEMA.COLUMNS as c 
				ON k.COLUMN_NAME = c.COLUMN_NAME
		WHERE k.TABLE_NAME = @Tabla AND k.CONSTRAINT_NAME LIKE '%FK%'
		ORDER BY c.ORDINAL_POSITION DESC);

		-- verificamos si existe... y pasamos a llamar al select.			
		DECLARE @var tablaVerificar;
		INSERT INTO @var(Tabla, NRelaciones, Campo, Valor) VALUES(@Tabla, @nRelaciones, @campo, @valor);
		DECLARE @resultado NVARCHAR(MAX) = (SELECT * FROM dbo.Verificar(@var, 0, @NumeroLlaves, @minimo, @maximo));

		INSERT INTO #resultado
		EXECUTE(@resultado);

		IF (SELECT resultado_id FROM #resultado) = 0 BEGIN
			SET @resultadoSalida = 'sin_coincidencia';
			SELECT 'sin_coincidencia' as error;
		END
		ELSE BEGIN 
			SET @resultado = (SELECT * FROM dbo.Verificar(@var, 1, @NumeroLlaves, @minimo, @maximo));
			EXECUTE(@resultado);
		END	

	END TRY
	BEGIN CATCH
		SELECT 'error' as error, ERROR_MESSAGE();
		SET @resultadoSalida = 'error';
	END CATCH
END
GO
/****** Object:  StoredProcedure [dbo].[ModuloSeguridad]    Script Date: 2/2/2024 12:31:12 AM ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE [dbo].[ModuloSeguridad] 
	@Email NVARCHAR(MAX),
	@Passwd NVARCHAR(MAX)
AS 
BEGIN 
	BEGIN TRY 
		DECLARE @valores NVARCHAR(MAX) = CONCAT('(', @Email, '),', '(', @Passwd, ')')
		DECLARE @resultado NVARCHAR(MAX);
		EXECUTE ListarPTabla @Tabla = 'usuarioPerfil', @nRelaciones = 2, @campo = 'usuario_correo, usuario_contrasena', @valor = @valores, @resultadoSalida = @resultado OUTPUT;	
	
		IF @resultado = 'sin_coincidencia' BEGIN 
			SELECT 'credenciales_incorrectas';
		END 

	END TRY 
	BEGIN CATCH 
		SELECT 'error';
	END CATCH 
END 
GO
USE [master]
GO
ALTER DATABASE [SQLDB_CONCESIONARIA] SET  READ_WRITE 
GO

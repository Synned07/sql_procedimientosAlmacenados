CREATE TYPE tablaVerificar AS TABLE (
	Tabla NVARCHAR(MAX) NOT NULL,
	NRelaciones INT DEFAULT 0,
	Campo NVARCHAR(MAX) DEFAULT '',
	Valor NVARCHAR(MAX) DEFAULT '',
	ResultadoFinal INT DEFAULT 0
);


GO
--Procedimiento almacenados Padre para Listar
ALTER PROCEDURE ListarPTabla
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

		-- verificamos si existe... y pasamos a llamar al select.			
		DECLARE @var tablaVerificar;
		INSERT INTO @var(Tabla, NRelaciones, Campo, Valor) VALUES(@Tabla, @nRelaciones, @campo, @valor);
		DECLARE @resultado NVARCHAR(MAX) = (SELECT * FROM dbo.Verificar(@var, 0, @NumeroLlaves));
		
		INSERT INTO #resultado
		EXECUTE(@resultado);

		IF (SELECT resultado_id FROM #resultado) = 0 BEGIN
			SELECT 'sin_coincidencia' as error;
		END
		ELSE BEGIN 
			--ejecutamos consulta.
			SET @resultado = (SELECT * FROM dbo.Verificar(@var, 1, @NumeroLlaves));
			EXECUTE(@resultado);
		END	

	END TRY
	BEGIN CATCH
		SELECT 'error' as error, ERROR_MESSAGE();
		SET @resultadoSalida = 'error';
	END CATCH
END

EXECUTE ListarPTabla @Tabla = 'Reserva';


GO

--Procedimiento almacenados Padre para Crear
ALTER PROCEDURE CrearPTabla
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
		
		SELECT @SQL;
		--EXECUTE( @SQL );
		--SELECT 'ok';
	END TRY
	BEGIN CATCH
		SELECT ERROR_MESSAGE();
	END CATCH
END


EXECUTE CrearPTabla 
				@TablaEntrada = 'reserva', 
				@NumeroCampos = 4, 
				@Esquema = 'modeloAutoId, calendarioid, pruebaManejoId, usuarioPerfil_id',  
				@Registros = '10, 59, 46, 35';


GO



--Procedimiento almacenados Padre para Eliminar
ALTER PROCEDURE EliminarPTabla
	@TablaEntrada NVARCHAR(MAX) = '',
	@key NVARCHAR(MAX) = '',
	@value NVARCHAR(MAX) = ''
AS
BEGIN
	BEGIN TRY 
		DECLARE @resultadoTexto NVARCHAR(MAX) = '';
		EXECUTE ListarPTabla @Tabla = @TablaEntrada, @nRelaciones = 1, @campo=@key, @valor=@value, @resultadoSalida = @resultadoTexto OUTPUT;

		IF (NOT @TablaEntrada = '') AND (NOT @key = '') AND (NOT @value = '') AND @resultadoTexto != 'sin_data' AND @resultadoTexto != 'error' BEGIN
			DECLARE @CMD NVARCHAR(MAX) = 'DELETE FROM';
			SET @CMD += ' ' + @TablaEntrada;
			SET @CMD += ' ' + 'WHERE';
			SET @CMD += ' ' + @key;
			SET @CMD += ' = ' + @value;

			EXECUTE(@CMD);
			SELECT 'ok';
		END
		ELSE BEGIN 
			SELECT 'error';
		END
	END TRY 
	BEGIN CATCH 
		SELECT 'error';
	END CATCH 
END

EXEC EliminarPTabla 
				@TablaEntrada = 'usuario', 
				@Key = 'cedula', 
				@value = '1111111111';



GO

--Procedimiento almacenados Padre para Actualizar
ALTER PROCEDURE ActualizarPTabla
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

		IF @resultadoTexto != 'sin_data' AND @resultadoTexto != 'error' BEGIN 

			DECLARE @CMD NVARCHAR(MAX);
			SET @CMD = N'UPDATE ' + @TablaEntrada + ' SET ';

			DECLARE @Relacion NVARCHAR(MAX) = dbo.FuncionString(@NRelaciones, @campos, @valor, 'actualizar');
			SET @CMD += @Relacion;

			IF @CampoCondicional != '' AND @ValorCondicional != '' AND @NCondicional != 0 BEGIN 
				SET @CMD += ' WHERE ';
				DECLARE @Condicional NVARCHAR(MAX) = dbo.FuncionString( @NCondicional, @CampoCondicional, @ValorCondicional, 'actualizar2');
				SET @CMD += @Condicional;
			END


			EXEC sp_sqlexec @CMD;
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


EXEC ActualizarPTabla 
			@TablaEntrada = 'metodoPago', 
			@NRelaciones = 2, 
			@Campos = 'Metodopago_tipo, MetodoPago_estado', 
			@Valor = 'trasferencia, habilitado', 
			@NCondicional = 1, 
			@CampoCondicional = 'Metodopago_id', 
			@ValorCondicional = '8';	


GO


/* =================================================== */
--			     FUNCION SUBSTRING                      --
/* =================================================== */


ALTER FUNCTION Verificar( @informacion tablaVerificar READONLY, @estado INT = 0 , @numeroLlaves INT)
	RETURNS @verificado TABLE( resultado NVARCHAR(MAX) )
AS
BEGIN 	
	DECLARE @sql NVARCHAR(MAX) = 'SELECT COUNT(*) ';
	
	IF @estado = 1 BEGIN 
		SET @sql = 'SELECT * ';
	END

	DECLARE @comprobacion INT = 1;

	DECLARE @Tabla_Entrada NVARCHAR(MAX) = (SELECT Tabla FROM @informacion);
	DECLARE @NRelaciones INT = (SELECT NRelaciones FROM @informacion);
	DECLARE @Campo NVARCHAR(MAX) = (SELECT Campo FROM @informacion);
	DECLARE @Valor NVARCHAR(MAX) = (SELECT Valor FROM @informacion);

	SET @sql += 'FROM ' + @Tabla_Entrada;

	IF @numeroLlaves > 1 
	BEGIN
		
	END 

	IF @Campo != '' AND @Valor != '' 
	BEGIN 
		SET @sql += ' WHERE ' + dbo.FuncionString(@NRelaciones, @Campo, @Valor, 'verificar');
	END
	
	INSERT INTO @verificado
	SELECT @sql;

	RETURN;
END

GO


DECLARE @consulta NVARCHAR(MAX) = 'SELECT * FROM reserva';
DECLARE @Tabla NVARCHAR(MAX) = 'reserva';

DECLARE @TablaSecundaria NVARCHAR(MAX);
DECLARE @Columna NVARCHAR(MAX);

SELECT @TablaSecundaria = REPLACE(K.CONSTRAINT_NAME, 'FK_'+@Tabla+'_', ''), 
		@Columna = K.COLUMN_NAME,
		@TablaSecundaria = REPLACE(@TablaSecundaria, '_'+@Columna, '')
FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE AS K 
	INNER JOIN INFORMATION_SCHEMA.COLUMNS AS C 
		ON K.COLUMN_NAME = C.COLUMN_NAME
WHERE K.TABLE_NAME = @Tabla AND K.CONSTRAINT_NAME LIKE '%FK%' AND C.ORDINAL_POSITION = 4;

IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE AS K WHERE K.TABLE_NAME = @TablaSecundaria AND K.CONSTRAINT_NAME LIKE '%FK%' ) 
BEGIN  
	DECLARE @EXIT INT = 1;
	WHILE @EXIT = 1 BEGIN 
		-- reafinar un poco mas esta parte del codigo.
		DECLARE @actualizarTabla NVARCHAR(MAX);
		SET @actualizarTabla = @TablaSecundaria;

		SELECT 
			@consulta += ' INNER JOIN ' + @TablaSecundaria + ' ON ' + @Tabla + '.' + @Columna + ' = ' + @TablaSecundaria + '.' + 
			( 
				SELECT TOP 1 COLUMN_NAME FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE WHERE TABLE_NAME = @TablaSecundaria AND CONSTRAINT_NAME LIKE '%PK%' 
			),
			@Tabla = @actualizarTabla,
			@Columna = K.COLUMN_NAME,
			@TablaSecundaria = (
				CASE
					WHEN COUNT(*) >= 1 THEN REPLACE(REPLACE(K.CONSTRAINT_NAME, 'FK_'+@TablaSecundaria+'_', ''), '_'+K.COLUMN_NAME, '') -- sacamos la otra tabla
					ELSE 	
						'no'
				END 
			)

		FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE AS K
			INNER JOIN INFORMATION_SCHEMA.COLUMNS AS C
				ON k.COLUMN_NAME = C.COLUMN_NAME
		WHERE K.TABLE_NAME = @TablaSecundaria AND K.CONSTRAINT_NAME LIKE '%FK%'
		GROUP BY K.CONSTRAINT_NAME, K.COLUMN_NAME;


		IF NOT EXISTS (SELECT * FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE AS K WHERE K.TABLE_NAME = @TablaSecundaria AND K.CONSTRAINT_NAME LIKE '%FK%' ) BEGIN
			SELECT @consulta += ' INNER JOIN ' + @TablaSecundaria + ' ON ' + @Tabla + '.' + @Columna + ' = ' + @TablaSecundaria + '.' + K.COLUMN_NAME
			FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE AS K
				INNER JOIN INFORMATION_SCHEMA.COLUMNS AS C
					ON k.COLUMN_NAME = C.COLUMN_NAME
			WHERE K.TABLE_NAME = @TablaSecundaria AND K.CONSTRAINT_NAME LIKE '%PK%' 
			SET @EXIT = 0; 
		END
	END

END 
ELSE IF @TablaSecundaria IS NOT NULL 
BEGIN 
	SELECT @consulta += ' INNER JOIN ' + @TablaSecundaria + ' ON ' + @Tabla + '.' + @Columna + ' = ' + @TablaSecundaria + '.' + K.COLUMN_NAME
	FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE AS K
		INNER JOIN INFORMATION_SCHEMA.COLUMNS AS C
			ON k.COLUMN_NAME = C.COLUMN_NAME
	WHERE K.TABLE_NAME = @TablaSecundaria AND K.CONSTRAINT_NAME LIKE '%PK%' 
END

SELECT @consulta;


GO


DECLARE @entrada tablaVerificar;
INSERT INTO @entrada(Tabla, NRelaciones, Campo, Valor) VALUES('reserva', 0, '', '');
SELECT * FROM Verificar(@entrada, 0, 6);

GO

CREATE FUNCTION CrearRelacion(@Indice INT, @Tabla_Entrada NVARCHAR(MAX)) 
	RETURNS NVARCHAR(MAX)
AS 
BEGIN 
	DECLARE @pk_name NVARCHAR(MAX) = '';
	DECLARE @nombre_columna NVARCHAR(MAX) = '';
	DECLARE @sql NVARCHAR(MAX);

	-- para verificar otras tablas distintas
	DECLARE @verificarTabla NVARCHAR(MAX);
	DECLARE @minimo INT = 1;
	DECLARE @maximo INT;

	SELECT TOP 1 @pk_name = REPLACE(K.CONSTRAINT_NAME, 'FK_'+@Tabla_Entrada+'_', ''), @nombre_columna = K.COLUMN_NAME
	FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE AS K 
		INNER JOIN INFORMATION_SCHEMA.COLUMNS AS C
			ON K.COLUMN_NAME = C.COLUMN_NAME
	WHERE K.TABLE_NAME = @Tabla_Entrada AND C.TABLE_NAME = @Tabla_Entrada AND K.CONSTRAINT_NAME LIKE '%FK%' AND C.ORDINAL_POSITION = @Indice;

	IF @pk_name != '' AND @nombre_columna != '' 
	BEGIN
		SET @pk_name = REPLACE(@pk_name, '_'+@nombre_columna, '');
		SET @pk_name = 'PK_'+@pk_name;


		SELECT @sql += ' INNER JOIN ' + CTU.TABLE_NAME + ' ON ' + @Tabla_Entrada + '.' + @nombre_columna + ' = ' + CTU.TABLE_NAME + '.' + KCU.COLUMN_NAME + ' ', 
			@verificarTabla = (
				SELECT 
					CASE
						WHEN COUNT(x.TABLE_NAME) != 1 THEN x.TABLE_NAME
						ELSE 
							'ninguno'
					END
				FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE AS x
				WHERE x.TABLE_NAME = CTU.TABLE_NAME
				GROUP BY x.TABLE_NAME
			),
			@maximo = (
				SELECT 
					COUNT(x.TABLE_NAME)
				FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE AS x
				WHERE x.TABLE_NAME = CTU.TABLE_NAME
				GROUP BY x.TABLE_NAME 
			)
		FROM INFORMATION_SCHEMA.CONSTRAINT_TABLE_USAGE AS CTU 
			INNER JOIN INFORMATION_SCHEMA.KEY_COLUMN_USAGE AS KCU
				ON CTU.TABLE_NAME = KCU.TABLE_NAME
		WHERE CTU.CONSTRAINT_NAME = @pk_name AND KCU.CONSTRAINT_NAME LIKE '%PK%';
		
	END



END

GO

-- funcion con valores de tabla
ALTER FUNCTION FuncionString(@NColumnas INT, @Columnas NVARCHAR(MAX), @Columnas2 NVARCHAR(MAX), @Tipo NVARCHAR(50)) 
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
				SET @CMD += ' OR ';
			END
		END
		
		SET @Indice += 1
	END
	
	RETURN @CMD;
END

GO

DECLARE @result NVARCHAR(MAX) = dbo.FuncionString(3,'relacion1, relacion2, relacion3', '(aqui comand,), (difjjdf,jdi,i), (escribiendo, aqui mi comentario)', 'verificar');
PRINT @result;

DECLARE @texto NVARCHAR(MAX) = 'difji)';
SELECT @texto, STUFF(@texto, LEN(SUBSTRING(@texto, 0, CHARINDEX(')', @texto, 0)))+1, 1, '');


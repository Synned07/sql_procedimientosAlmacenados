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

EXECUTE ListarPTabla @Tabla = 'usuario', @nRelaciones = 1, @campo = 'usuario_id', @valor = '(120)';	
EXECUTE ListarPTabla @Tabla = 'reserva';

SELECT * FROM perfil;

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
		
		EXECUTE( @SQL );
		SELECT 'ok';
	END TRY
	BEGIN CATCH
		SELECT 'error', ERROR_MESSAGE();
	END CATCH
END


EXECUTE CrearPTabla 
				@TablaEntrada = 'usuario', 
				@NumeroCampos = 6, 
				@Esquema = 'usuario_nombre, usuario_apellido, usuario_cedula, usuario_celular, usuario_correo, usuario_contrasena',  
				@Registros = '(est), (phoni), (1023222098), (114448998), (est@hotmail.com), (JFJJJ9-JJJ90)';


SELECT * FROM usuario;

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

EXEC EliminarPTabla 
				@TablaEntrada = 'usuario', 
				@Key = 'usuario_id', 
				@value = '(28)';



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


EXEC ActualizarPTabla 
			@TablaEntrada = 'metodoPago', 
			@NRelaciones = 2, 
			@Campos = 'MetodoPago_tipo, MetodoPago_estado', 
			@Valor = '(2), (0)', 
			@NCondicional = 1, 
			@CampoCondicional = 'Metodopago_id', 
			@ValorCondicional = '(3)';	

SELECT * FROM MetodoPago;


GO


GO
/* =================================================== */
--			     FUNCION SUBSTRING                      --
/* =================================================== */


ALTER FUNCTION Verificar( @informacion tablaVerificar READONLY, @estado INT = 0 , @numeroLlaves INT, @minimoEntrada INT = 0, @maximoEntrada INT = 0)
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

		DECLARE @guardarTablaOriginal NVARCHAR(MAX);
		SET @guardarTablaOriginal = @Tabla_Entrada;

		WHILE @minimoEntrada <= @maximoEntrada BEGIN 
			
			DECLARE @TablaSecundaria NVARCHAR(MAX);
			DECLARE @Columna NVARCHAR(MAX);

			SELECT @TablaSecundaria = REPLACE(K.CONSTRAINT_NAME, 'FK_'+@Tabla_Entrada+'_', ''), 
					@Columna = K.COLUMN_NAME,
					@TablaSecundaria = REPLACE(@TablaSecundaria, '_'+@Columna, '')
			FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE AS K 
				INNER JOIN INFORMATION_SCHEMA.COLUMNS AS C 
					ON K.COLUMN_NAME = C.COLUMN_NAME
			WHERE K.TABLE_NAME = @Tabla_Entrada AND K.CONSTRAINT_NAME LIKE '%FK%' AND C.ORDINAL_POSITION = @minimoEntrada;


			IF EXISTS (SELECT * FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE AS K WHERE K.TABLE_NAME = @TablaSecundaria AND K.CONSTRAINT_NAME LIKE '%FK%' ) 
			BEGIN  
				DECLARE @EXIT INT = 1;
				WHILE @EXIT = 1 BEGIN -- este bucle debe captar todas las foraneas existentes.

					-- reafinar un poco mas esta parte del codigo.
					DECLARE @Minimo INT = (SELECT TOP 1 C.ORDINAL_POSITION
					FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE AS K
						INNER JOIN INFORMATION_SCHEMA.COLUMNS AS C
							ON k.COLUMN_NAME = C.COLUMN_NAME
					WHERE K.TABLE_NAME = @TablaSecundaria AND K.CONSTRAINT_NAME LIKE '%FK%' 
					ORDER BY C.ORDINAL_POSITION ASC);

					DECLARE @Maximo INT = (SELECT TOP 1 C.ORDINAL_POSITION
					FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE AS K
						INNER JOIN INFORMATION_SCHEMA.COLUMNS AS C
							ON k.COLUMN_NAME = C.COLUMN_NAME
					WHERE K.TABLE_NAME = @TablaSecundaria AND K.CONSTRAINT_NAME LIKE '%FK%' 
					ORDER BY C.ORDINAL_POSITION DESC);

					DECLARE @actualizarTabla NVARCHAR(MAX);
					SET @actualizarTabla = @TablaSecundaria;

					DECLARE @actualizarConstraint NVARCHAR(MAX);
					SET @actualizarConstraint = @TablaSecundaria;

					-- este es importante nos ayudara a verificar si existe otras relaciones 
					DECLARE @verificarRelaciones INT = 0;

					WHILE @Minimo <= @Maximo BEGIN 
				
						SELECT 
							@sql += (
								CASE
									WHEN @actualizarTabla = @TablaSecundaria THEN
									' INNER JOIN ' + @TablaSecundaria + ' ON ' + @Tabla_Entrada + '.' + @Columna + ' = ' + @TablaSecundaria + '.' + 
										( 
											SELECT TOP 1 COLUMN_NAME FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE WHERE TABLE_NAME = @TablaSecundaria AND CONSTRAINT_NAME LIKE '%PK%' 
										)
									ELSE 
										''
								END 
							),
							@Tabla_Entrada = @actualizarTabla,
							@Columna = K.COLUMN_NAME,
							@TablaSecundaria = (
								CASE
									WHEN COUNT(*) >= 1 THEN REPLACE(REPLACE(K.CONSTRAINT_NAME, 'FK_'+@actualizarConstraint+'_', ''), '_'+K.COLUMN_NAME, '') -- sacamos la otra tabla
									ELSE 	
										'no'
								END 
							)

						FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE AS K
							INNER JOIN INFORMATION_SCHEMA.COLUMNS AS C
								ON k.COLUMN_NAME = C.COLUMN_NAME
						WHERE K.TABLE_NAME = @actualizarTabla AND K.CONSTRAINT_NAME LIKE '%FK%' AND C.ORDINAL_POSITION = @Minimo
						GROUP BY K.CONSTRAINT_NAME, K.COLUMN_NAME, C.ORDINAL_POSITION
						ORDER BY C.ORDINAL_POSITION ASC;


						IF NOT EXISTS (SELECT * FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE AS K WHERE K.TABLE_NAME = @TablaSecundaria AND K.CONSTRAINT_NAME LIKE '%FK%' ) BEGIN
							SELECT @sql += ' INNER JOIN ' + @TablaSecundaria + ' ON ' + @Tabla_Entrada + '.' + @Columna + ' = ' + @TablaSecundaria + '.' + K.COLUMN_NAME
							FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE AS K
								INNER JOIN INFORMATION_SCHEMA.COLUMNS AS C
									ON k.COLUMN_NAME = C.COLUMN_NAME
							WHERE K.TABLE_NAME = @TablaSecundaria AND K.CONSTRAINT_NAME LIKE '%PK%' 

						END
						ELSE 
						BEGIN 
							SET @verificarRelaciones = 1;
						END

						IF (@verificarRelaciones = 0) AND (@Minimo = @Maximo)
						BEGIN 
							SET @EXIT = 0; 
						END 	
						
						SET @Minimo += 1;
					END

				END

			END 
			ELSE IF @TablaSecundaria IS NOT NULL 
			BEGIN 
				SELECT @sql += ' INNER JOIN ' + @TablaSecundaria + ' ON ' + @Tabla_Entrada + '.' + @Columna + ' = ' + @TablaSecundaria + '.' + K.COLUMN_NAME
				FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE AS K
					INNER JOIN INFORMATION_SCHEMA.COLUMNS AS C
						ON k.COLUMN_NAME = C.COLUMN_NAME
				WHERE K.TABLE_NAME = @TablaSecundaria AND K.CONSTRAINT_NAME LIKE '%PK%' 
			END

			SET @minimoEntrada += 1;
			SET @Tabla_Entrada = @guardarTablaOriginal;
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

DECLARE @entrada tablaVerificar;
INSERT INTO @entrada(Tabla, NRelaciones, Campo, Valor) VALUES('reserva', 0, '', '');
SELECT * FROM Verificar(@entrada, 0, 6, 0, 0);


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


GO


SELECT * FROM auto;
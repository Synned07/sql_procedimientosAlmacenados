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

SELECT * FROM reserva;

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
				@TablaEntrada = 'carroceria', 
				@Key = 'carroceria_id', 
				@value = '(21)';


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


/* ==================================================== */
-- 				PROCEDIMIENTO ALMACENADO PARA LOGIN
/* ==================================================== */
GO

ALTER PROCEDURE ModuloSeguridad 
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


EXEC ModuloSeguridad @Email = 'juan_gomez@example.com', @Passwd = 'A289424F-2958-4';
SELECT * FROM usuario;


exec sp_columns perfil;

/* =================================================== */
--			     FUNCION SUBSTRING                      --
/* =================================================== */

GO

ALTER FUNCTION Verificar( @informacion tablaVerificar READONLY, @estado INT = 0 , @numeroLlaves INT, @minimo INT = 0, @maximo INT = 0)
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
				
				SELECT @sql += ' INNER JOIN ' + @TablaSecundaria + ' ON (' + @tabla + '.' + @Columna + ' = ' + c.TABLE_NAME + '.' + c.COLUMN_NAME + ')'
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
				
							SELECT @sql += ' INNER JOIN ' + @otraTabla + ' ON (' + @TablaSecundaria + '.' + @Columna2 + ' = ' + c.TABLE_NAME + '.' + c.COLUMN_NAME + ')'
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

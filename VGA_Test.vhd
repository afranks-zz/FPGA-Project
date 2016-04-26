-------------------------------------------------------------------------------
--
-- Project					: VGA_Test
-- File name				: VGA_Test.vhd
-- Title						: VGA display test 
-- Description				:  
--								: 
-- Design library			: N/A
-- Analysis Dependency	: VGA_SYNC.vhd
-- Simulator(s)			: ModelSim-Altera version 6.1g
-- Initialization			: none
-- Notes						: This model is designed for synthesis
--								: Compile with VHDL'93
--
-------------------------------------------------------------------------------
--
-- Revisions
--			Date		Author			Revision		Comments
--		3/11/2008		W.H.Robinson	Rev A			Creation
--		3/13/2012		W.H.Robinson	Rev B			Update for DE2-115 Board
--
--			
-------------------------------------------------------------------------------

-- Always specify the IEEE library in your design


LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.all;
LIBRARY work;
USE work.roms.all;
--USE ieee.std_logic_unsigned.ALL;
--USE  IEEE.STD_LOGIC_ARITH.all;


-- Entity declaration
-- 		Defines the interface to the entity

ENTITY VGA_Test IS


	PORT
	(
-- 	Note: It is easier to identify individual ports and change their order
--	or types when their declarations are on separate lines.
--	This also helps the readability of your code.

    -- Clocks
    
    CLOCK_50	: IN STD_LOGIC;  -- 50 MHz
 
    -- Buttons 
    
    KEY 		: IN STD_LOGIC_VECTOR (3 downto 0);         -- Push buttons

    -- Input switches
    
    SW 			: IN STD_LOGIC_VECTOR (17 downto 0);         -- DPDT switches

    -- VGA output
    
   VGA_BLANK_N 	: out std_logic;            -- BLANK
   VGA_CLK 	 		: out std_logic;            -- Clock
   VGA_HS 		 	: out std_logic;            -- H_SYNC
   VGA_SYNC_N  	: out std_logic;            -- SYNC
   VGA_VS 		 	: out std_logic;            -- V_SYNC
   VGA_R 		 	: out unsigned(7 downto 0); -- Red[7:0]
   VGA_G 		 	: out unsigned(7 downto 0); -- Green[7:0]
   VGA_B 		 	: out unsigned(7 downto 0) -- Blue[7:0]


	);
END VGA_Test;


-- Architecture body 
-- 		Describes the functionality or internal implementation of the entity

ARCHITECTURE structural OF VGA_Test IS

type charDefArray is array (0 to 18) of std_LOGIC_vector (0 to 48);
type charDisArray is array (0 to 22) of unsigned (4 downto 0);

-- Defines the available character set. Very limited. 
constant characterArray : charDefArray := ("0011100010001001001100101010011001001000100011100", "0001000001100000010000001000000100000010000011100",
"0011100010001000000100000100000100000100000111110", "0111110000010000010000000100000001001000100011100", "0000100000110000101000100100011111000001000000100",
"0111110010000001111000000010000001001000100011100", "0001100001000001000000111100010001001000100011100", "0111110000001000001000001000001000000100000010000",
"0011100010001001000100011100010001001000100011100","0011100010001001000100011110000001000001000011000",
"0000000001100000110000000000001100000110000000000",
"0000000000000000000000111110000000000000000000000","0011110010000001000000011100000001000000100111100","0011100010001001000000100000010000001000100011100",
"0011100010001001000100100010010001001000100011100", "0111100010001001000100111100010100001001000100010", "0111110010000001000000111100010000001000000111110",
"0100010010001001000100111110010001001000100100010","0011100000100000010000001000000100000010000011100");
constant floorPos : unsigned(11 downto 0) := to_unsigned(416,12);
constant floorObsHeight : unsigned(9 downto 0) := to_unsigned(32,10);
constant skyObsHeight : unsigned(9 downto 0) := to_unsigned(32,10);
constant floorObsWidth : unsigned(11 downto 0) := to_unsigned(32,12);
constant skyObsWidth : unsigned(11 downto 0) := to_unsigned(64,12);
constant playerJumpForce : std_logic_vector(11 downto 0) := STD_LOGIC_VECTOR(to_signed(-32,12));
constant downwardAccel : std_logic_vector(11 downto 0) := STD_LOGIC_VECTOR(to_signed(2,12));
 
COMPONENT VGA_SYNC_module

	PORT(	clock_50Mhz 	: IN	STD_LOGIC;
			horiz_sync_out, vert_sync_out, video_on, pixel_clock : OUT	STD_LOGIC;
				red, green, blue	: IN STD_LOGIC_VECTOR(7 DOWNTO 0);
				red_out, green_out, blue_out : out  unsigned(7 DOWNTO 0);
				pixel_row: OUT STD_LOGIC_VECTOR(9 DOWNTO 0);
			pixel_column: OUT STD_LOGIC_VECTOR(9 DOWNTO 0));
			

END COMPONENT;


SIGNAL red_int : STD_LOGIC_VECTOR(7 DOWNTO 0);
SIGNAL green_int : STD_LOGIC_VECTOR(7 DOWNTO 0);
SIGNAL blue_int : STD_LOGIC_VECTOR(7 DOWNTO 0);
SIGNAL video_on_int : STD_LOGIC; 
SIGNAL pixel_clock_int : STD_LOGIC;
SIGNAL pixel_row_int :STD_LOGIC_VECTOR(9 DOWNTO 0); 
SIGNAL pixel_column_int :STD_LOGIC_VECTOR(9 DOWNTO 0);
SIGNAL playerVelocity :STD_LOGIC_VECTOR(11 DOWNTO 0) := STD_LOGIC_VECTOR(to_unsigned(0,12));
SIGNAL playerYPos : STD_LOGIC_VECTOR (11 DOWNTO 0) := STD_LOGIC_VECTOR(floorPos);
SIGNAL floorObsXPos,skyObsXPos : STD_LOGIC_VECTOR (11 DOWNTO 0) := STD_LOGIC_VECTOR(to_unsigned(640,12));
SIGNAL floorObsInitVelocity : STD_LOGIC_VECTOR (11 DOWNTO 0) := STD_LOGIC_VECTOR(to_unsigned(-6,12)); --neg
SIGNAL skyObsInitVelocity : STD_LOGIC_VECTOR (11 DOWNTO 0) := STD_LOGIC_VECTOR(to_unsigned(-6,12)); --neg (6)
SIGNAL BGVelocity : STD_LOGIC_VECTOR (4 DOWNTO 0) := STD_LOGIC_VECTOR(to_unsigned(6,5)); --pos
SIGNAL floorObsVelocity, skyObsVelocity : STD_LOGIC_VECTOR (11 DOWNTO 0) := STD_LOGIC_VECTOR(to_unsigned(0,12));
SIGNAL Key0Prev, Key1Prev, key2prev, key3prev  : STD_LOGIC := '0';
SIGNAL vert_sync_int : STD_LOGIC;
SIGNAL horiz_sync_int : STD_LOGIC; 
SIGNAL jump, triggerObs, triggerSkyObs : std_logic := '0';
SIGNAL CLK_cOUNT_1HZ : std_logic_vector(27 downto 0);
signal clk_1HZ_Enable, playing, triggerPlaying : std_logic := '0';
signal scoreTime, hiScoreTime: std_logic_vector (13 downto 0);
signal halfSize, lastFloor: std_logic := '0';
signal playerHeight: std_logic_vector (9 downto 0) := std_logic_vector(to_unsigned(64,10));
signal playerLeftCoord : std_logic_vector(11 downto 0) := std_logic_vector(to_unsigned(128,12));
signal playerWidth : std_logic_vector(7 downto 0) := std_logic_vector(to_unsigned(64,8));
signal backShift : std_logic_vector(4 downto 0) := std_logic_vector(to_unsigned(0,5));



-- Default values for display 
SIGNAL characterDisplay : charDisArray := (to_unsigned(12,5),to_unsigned(13,5),to_unsigned(14,5),to_unsigned(15,5),to_unsigned(16,5),to_unsigned(10,5),to_unsigned(0,5),to_unsigned(0,5),to_unsigned(0,5),to_unsigned(0,5),to_unsigned(17,5),to_unsigned(18,5),to_unsigned(11,5),to_unsigned(12,5),
to_unsigned(13,5),to_unsigned(14,5),to_unsigned(15,5),to_unsigned(16,5),to_unsigned(10,5),to_unsigned(0,5),to_unsigned(0,5),to_unsigned(0,5),to_unsigned(0,5));


BEGIN

	--VGA_R(6 DOWNTO 0) <= "0000000";
	--VGA_G(6 DOWNTO 0) <= "0000000";
	--VGA_B(6 DOWNTO 0) <= "0000000";
	red_int(3 downto 0) <= "0000";
	blue_int(3 downto 0) <= "0000";
	green_int(3 downto 0) <= "0000";
	VGA_HS <= horiz_sync_int;
	VGA_VS <= vert_sync_int;

process (pixel_row_int,pixel_column_int)
	variable xPos :unsigned(7 downto 0);
	variable yPos : unsigned(7 downto 0);
	variable whichChar : unsigned(6 downto 0);
	variable shiftBy : unsigned(4 downto 0);
	-- Graphics
begin
			
		 -- Player
		if(unsigned(pixel_column_int) > unsigned(playerLeftCoord) AND unsigned(pixel_column_int) <= unsigned(playerLeftCoord) + unsigned(playerWidth) AND unsigned(PIXEL_row_int) >= (unsigned(playerYPos) - unsigned(playerHeight)) AND unsigned(pixel_row_int) < unsigned(playerYPos)) then
			xPos := unsigned("00011111" and unsigned(pixel_column_int(7 downto 0)) - unsigned(playerleftcoord(7 downto 0)));
			yPos := unsigned("00011111" and unsigned(pixel_row_int(7 downto 0)) - unsigned(playerYPos(7 downto 0)));
			
			red_int(7 downto 4) <= player(to_integer(yPos * 32) + to_integer(xPos))(0 to 3);
			green_int(7 downto 4) <= player(to_integer(yPos * 32) + to_integer(xPos))(4 to 7);
			blue_int(7 downto 4) <= player(to_integer(yPos * 32) + to_integer(xPos))(8 to 11);
-- Floor obst.	
		elsif(unsigned(pixel_row_int) < unsigned(floorPos) and unsigned(pixel_row_int) >= unsigned(floorPos) - unsigned(floorObsHeight) and to_integer(unsigned(pixel_column_int)) > to_integer(signed(floorObsXPos)) and to_integer(unsigned(pixel_column_int)) <= to_integer(signed(floorObsXPos)) + to_integer(floorObsWidth)) then
			xPos := unsigned("00011111" and unsigned(pixel_column_int(7 downto 0)) - unsigned(floorObsxPos(7 downto 0)));
			yPos := unsigned("00011111" and pixel_row_int(7 downto 0));
			
			red_int(7 downto 4) <= FloorObs(to_integer(yPos * 32) + to_integer(xPos))(0 to 3);
			green_int(7 downto 4) <= FloorObs(to_integer(yPos * 32) + to_integer(xPos))(4 to 7);
			blue_int(7 downto 4) <= FloorObs(to_integer(yPos * 32) + to_integer(xPos))(8 to 11);
-- Sky obst.	
		elsif(unsigned(pixel_row_int) < unsigned(floorPos) - unsigned(skyObsHeight) and unsigned(pixel_row_int) > 31 and to_integer(unsigned(pixel_column_int)) > to_integer(signed(skyObsXPos)) and to_integer(unsigned(pixel_column_int))<= to_integer(signed(skyObsXPos)) + to_integer(skyObsWidth)) then
			xPos := unsigned("00011111" and unsigned(pixel_column_int(7 downto 0)) - unsigned(skyObsxPos(7 downto 0)));
			yPos := unsigned("00011111" and pixel_row_int(7 downto 0));
			
			red_int(7 downto 4) <= SkyObs(to_integer(yPos * 32) + to_integer(xPos))(0 to 3);
			green_int(7 downto 4) <= SkyObs(to_integer(yPos * 32) + to_integer(xPos))(4 to 7);
			blue_int(7 downto 4) <= SkyObs(to_integer(yPos * 32) + to_integer(xPos))(8 to 11);
-- Floor	
		elsif(unsigned(pixel_row_int) >= unsigned(floorPos) - 16) then
		
			xPos := unsigned("00011111" and unsigned(pixel_column_int(7 downto 0)) - unsigned(backShift));
			yPos := unsigned("00011111" and pixel_row_int(7 downto 0));
			
			red_int(7 downto 4) <= Floor(to_integer(yPos * 32) + to_integer(xPos))(0 to 3);
			green_int(7 downto 4) <= Floor(to_integer(yPos * 32) + to_integer(xPos))(4 to 7);
			blue_int(7 downto 4) <= Floor(to_integer(yPos * 32) + to_integer(xPos))(8 to 11);
	

			
-- Text
		elsif(unsigned(pixel_row_int) < 32 and unsigned(pixEL_row_int) >= 12) then
			-- Characters
			-- Find our 0,0 position
			
			if(unsigned(pixel_column_int) >= 528) theN
				whichChar := unsigned(pixel_column_int(9 downto 3)) - 56;
			else
				whichChar := unsigned(pixel_column_int(9 downto 3)) - 1; -- Purposeful overflow
			end if;
			
			xPos := unsigned("00000111" and pixel_column_int(7 downto 0));
			yPos := unsigned(pixel_row_int(7 downto 0)) - 12;
			
			
			if(xPos > 6 or yPos > 6 or (whichChar > 9 and unsigned(pixel_column_int) < 528) or whichchar > 22) then
				xPos := unsigned("00011111" and pixel_column_int(7 downto 0));
				yPos := unsigned("00011111" and pixel_row_int(7 downto 0));
			
				red_int(7 downto 4) <= Top(to_integer(yPos * 32) + to_integer(xPos))(0 to 3);
				green_int(7 downto 4) <= Top(to_integer(yPos * 32) + to_integer(xPos))(4 to 7);
				blue_int(7 downto 4) <= Top(to_integer(yPos * 32) + to_integer(xPos))(8 to 11);
		
			
			--elsif(characterArray(to_integer(characterDisplay(to_integer(whichChar))))(to_integer(yPos*6)+to_integer(xPos)) = '1') then
			elsif(characterArray(to_integer(unsigned(characterDisplay(to_integer(whichChar)))))(to_integer(yPos*7)+to_integer(xPos)) = '1') then
				red_int(7 downto 4) <= "0000";
				blue_int(7 downto 4) <= "0000";
				green_int(7 downto 4) <= "0000";
			else
				xPos := unsigned("00011111" and pixel_column_int(7 downto 0));
				yPos := unsigned("00011111" and pixel_row_int(7 downto 0));
			
				red_int(7 downto 4) <= Top(to_integer(yPos * 32) + to_integer(xPos))(0 to 3);
				green_int(7 downto 4) <= Top(to_integer(yPos * 32) + to_integer(xPos))(4 to 7);
				blue_int(7 downto 4) <= Top(to_integer(yPos * 32) + to_integer(xPos))(8 to 11);
			end if;
		elsif(unsigned(pixel_row_int) < 32) then
			xPos := unsigned("00011111" and pixel_column_int(7 downto 0));
			yPos := unsigned("00011111" and pixel_row_int(7 downto 0));
			
			red_int(7 downto 4) <= Top(to_integer(yPos * 32) + to_integer(xPos))(0 to 3);
			green_int(7 downto 4) <= Top(to_integer(yPos * 32) + to_integer(xPos))(4 to 7);
			blue_int(7 downto 4) <= Top(to_integer(yPos * 32) + to_integer(xPos))(8 to 11);
			
-- Background
		else
			xPos := unsigned("00011111" and unsigned(pixel_column_int(7 downto 0)) - unsigned(backShift));
			yPos := unsigned("00011111" and pixel_row_int(7 downto 0));
			
			red_int(7 downto 4) <= BG(to_integer(yPos * 32) + to_integer(xPos))(0 to 3);
			green_int(7 downto 4) <= BG(to_integer(yPos * 32) + to_integer(xPos))(4 to 7);
			blue_int(7 downto 4) <= BG(to_integer(yPos * 32) + to_integer(xPos))(8 to 11);
		end if;
end process;

process(vert_sync_int)
variable floorObsNewPos : STD_LOGIC_VECTOR (11 DOWNTO 0) := std_logic_vector(signed(floorObsXPos) + signed(floorObsVelocity));
variable skyObsNewPos : STD_LOGIC_VECTOR (11 DOWNTO 0) := std_logic_vector(signed(skyObsXPos) + signed(skyObsVelocity));
variable backShiftNew : STD_LOGIC_VECTOR (4 DOWNTO 0) := std_logic_vector(unsigned(backShift) - unsigned(BGVelocity))(4 downto 0);
variable playerNewYPos : STD_LOGIC_VECTOR (11 DOWNTO 0) := std_logic_vector(signed(playerYPos) + signed(playerVelocity));
variable playerNewHeight : STD_LOGIC_VECTOR (9 DOWNTO 0) := playerHeight;
-- Stuff that happens every frame
begin
	if(vert_sync_int= '1') then
		
			
			
		if(playing = '1') then
			backShift <= backShiftNew;
			if(halfSize = '1' and unsigned(playerHeight) > 32) then
			playerNewHeight := std_logic_vector(unsigned(playerHeight) - to_unsigned(5,10));
			playerHeight <= std_logic_vector(unsigned(playerHeight) - to_unsigned(5,10));
			
		elsif(halfSize = '0' and unsigned(playerHeight) < 64) then
		playerNewHeight :=  std_logic_vector(unsigned(playerHeight) + to_unsigned(5,10));
			playerHeight <= std_logic_vector(unsigned(playerHeight) + to_unsigned(5,10));
		end if;
		playerYPos <= playerNewYPos;
			if(jump = '1' and unsigned(playerYPos) = unsigned(floorPos)) then
				playerVelocity <= playerJumpForce;
			elsif(signed(playerYPos) + signed(playerVelocity) > signed(floorPos)) then
				playerVelocity <= STD_LOGIC_VECTOR(to_signed(0,12));
				playerYPos <= STD_LOGIC_VECTOR(floorPos);
			elsif(unsigned(playerYPos) < unsigned(floorPos)) then
				playerVelocity <= std_logic_vector(signed(playerVelocity) + signed(downwardAccel));
			end if;
			floorObsXPos <= floorObsNewPos;
			skyObsXPos <= skyObsNewPos;
			
			
			
			if(triggerObs = '1') then
				floorObsVelocity <= floorObsInitVelocity;
			elsif(signed(floorObsxPos) + signed(floorObsVelocity) < -100) then
				floorObsVelocity <= STD_LOGIC_VECTOR(to_signed(0,12));
				floorObsXPos <= STD_LOGIC_VECTOR(to_unsigned(640,12));
			end if;
			
			if(triggerSkyObs = '1') then
				skyObsVelocity <= skyObsInitVelocity;
			elsif(signed(skyObsxPos) + signed(skyObsVelocity) < -100) then
				skyObsVelocity <= STD_LOGIC_VECTOR(to_signed(0,12));
				skyObsXPos <= STD_LOGIC_VECTOR(to_unsigned(640,12));
			end if;
		end if;
		if(triggerPlaying = '1') theN
			playing <= '1';
			playerVelocity <= STD_LOGIC_VECTOR(to_signed(0,12));
			playerYPos <= STD_LOGIC_VECTOR(floorPos);
			playerHeight <= STD_LOGIC_VECTOR(to_signed(64,10));
			floorObsXPos <= STD_LOGIC_VECTOR(to_unsigned(640,12));
			skyObsXPos <= STD_LOGIC_VECTOR(to_unsigned(640,12));
		elsif(playing = '1'  and ((unsigned(floorObsNewPos) >= unsigned(playerLeftCoord) and unsigned(floorObsNewPos) <= unsigned(playerLeftCoord) + unsigned(playerWidth)) or (unsigned(floorObsNewPos) + unsigned(floorObsWidth) >=  unsigned(playerLeftCoord) and unsigned(floorObsNewPos) + unsigned(floorObsWidth) <= unsigned(playerLeftCoord) + unsigned(playerWidth)))) then
			if(unsigned(playerNewYPos) >= unsigned(floorPos) - unsigned(floorObsHeight) ) then -- Check for intesection on the vert.
				floorObsVelocity <= STD_LOGIC_VECTOR(to_signed(0,12));
				skyObsVelocity <= STD_LOGIC_VECTOR(to_signed(0,12));
				if(unsigned(floorObsXPos) > unsigned(playerLeftCoord) + unsigned(playerWidth)) then
					floorObsXPos <= STD_LOGIC_VECTOR(unsigned(playerLeftCoord) + unsigned(playerWidth));
					skyObsXPos <= skyObsXPos;
				end if;
				if(unsigned(playerNewYPos) > unsigned(floorPos) - unsigned(floorObsHeight) and unsigned(playerYPos) < unsigned(floorPos) - unsigned(floorObsHeight)) then
					playerYPos <= STD_LOGIC_VECTOR(unsigned(floorPos) - unsigned(floorObsHeight));
				end if;
				
				playerVelocity <= STD_LOGIC_VECTOR(to_signed(0,12));
			--playerYPos <= STD_LOGIC_VECTOR(to_unsigned(395,12));
				playing <= '0';

				if(unSigned(scoreTime) > unsigned(hiscoreTime)) theN
					hiscoretime <= scoretime;
					characterDisplay(19 to 22) <= characterDisplay(6 to 9);
				end if;
				backShift <= backShift;
			end if;
			
		elsif(playing = '1'  and ((unsigned(skyObsNewPos) >= unsigned(playerLeftCoord) and unsigned(skyObsNewPos) <= unsigned(playerLeftCoord) + unsigned(playerWidth)) or (unsigned(skyObsNewPos) + unsigned(skyObsWidth) >=  unsigned(playerLeftCoord) and unsigned(skyObsNewPos) + unsigned(skyObsWidth) <= unsigned(playerLeftCoord) + unsigned(playerWidth)))) then
			if((unsigned(playerNewYPos) < unsigned(floorPos) - unsigned(skyObsHeight)) or 
			(unsigned(playerNewYPos) - unsigned(playerNewHeight) < unsigned(floorPos) - unsigned(skyObsHeight))) then -- Check for intesection on the vert.
				floorObsVelocity <= STD_LOGIC_VECTOR(to_signed(0,12));
				skyObsVelocity <= STD_LOGIC_VECTOR(to_signed(0,12));
				if(unsigned(skyObsXPos) > unsigned(playerLeftCoord) + unsigned(playerWidth)) then
					floorObsXPos <= floorObsXPos;
					skyObsXPos <= STD_LOGIC_VECTOR(unsigned(playerLeftCoord) + unsigned(playerWidth));
				end if;
				playerVelocity <= STD_LOGIC_VECTOR(to_signed(0,12));
				--playerYPos <= STD_LOGIC_VECTOR(to_unsigned(395,12));
				playing <= '0';
				if(unsigned(skyObsXPos) < unsigned(playerLeftCoord) + unsigned(playerWidth) or unsigned(skyObsXPos) < unsigned(playerLeftCoord)) then
					playerHeight <= std_logic_vector(skyObsHeight);
				
				end if;
				if(unSigned(scoreTime) > unsigned(hiscoreTime)) theN
					hiscoretime <= scoretime;
					characterDisplay(19 to 22) <= characterDisplay(6 to 9);
				end if;
				backShift <= backShift;
			
			end if;
			
			
		end if;
		

	end if;
	
end process;



process(clock_50)
begin
	if(rising_edge(clocK_50)) then
		IF unsigned(CLK_COUNT_1HZ) < X"2FAF080" THEN 
			CLK_COUNT_1HZ <= std_logic_vector(unsigned(CLK_COUNT_1HZ) + to_unsigned(1,28));
			CLK_1HZ_Enable <= '0';
		ELSE
			CLK_COUNT_1HZ <= X"0000000";
			CLK_1HZ_Enable <= '1';
		END IF;
		if(playing = '1') then
		-- Obstacble triggering
			if(clk_1hZ_Enable = '1') then
				if(lastFloor = '0') then
					if(unsigned(floorObsXPos) = 640 and unsigned(skyObsXPos) = 640) theN
						triggerObs <= '1';
					elsif(signed(floorObsVelocity) /= 0) then
						triggerObs <= '0';
					end if;
					lastFloor <= '1';
				else
					if(unsigned(skyObsXPos) = 640 and unsigned(floorObsXPos) = 640) theN
						triggerSkyObs <= '1';
					elsif(signed(skyObsVelocity) /= 0) then
						triggerSkyObs <= '0';
					end if;
					lastFloor <= '0';
				end if;
				scoreTime <= std_logic_vector(unsigned(scoreTime) + 1);

				-- Update display;
				characterDisplay(9) <= characterDisplay(9) + 1;
				if(unsigned(characterDisplay(9)) = 9) theN
					characterDisplay(9) <= to_unsigned(0,5);
					characterDisplay(8) <= characterDisplay(8) + 1;
					if(unsigned(characterDisplay(8)) = 9) theN
						characterDisplay(8) <= to_unsigned(0,5);
						characterDisplay(7) <= characterDisplay(7) + 1;
							if(unsigned(characterDisplay(7)) = 9) theN
								characterDisplay(7) <= to_unsigned(0,5);
								characterDisplay(6) <= characterDisplay(6) + 1;
							end if;
					end if;
				end if;	
			end if;
		else
				scoretime <= std_logic_vector(to_unsigned(0,14));
				characterDisplay(6) <= "00000";
				characterDisplay(7) <= "00000";
				characterDisplay(8) <= "00000";
				characterDisplay(9) <= "00000";
			
		end if;
	end if;
end process;

-- Button processing
process (key(3 downto 0), clock_50)
begin
	if(rising_edge(clock_50)) then
	
		if(playing = '1') then
			if(key(1) = '0') then
				halfSize <= '1';
			else
				halfSize <= '0';
			end if;
		if(key(0) = '0') then
			if(unsigned(playerYPos) = floorPos and unsigned(playerHeight) = 64) then
					jump <= '1';
			elsif(signed(playerVelocity) /= 0) then
				jump <= '0';
			end if;
		end if;
			--key0prev <= key(0);
			--if(key(0) = '0' AND key0prev = '1') then
			--	key0prev <= '0';		
			key1prev <= key(1);
			if(key(1) = '0' AND key1prev = '1') then
				key1prev <= '0';
				--triggerObs <= '1';
			elsif(signed(floorObsVelocity) /= 0) then
				--triggerObs <= '0';
			end if;
		else
			--triggerObs <= '0';
			--jump <= '0';
		end if;
		
		key2prev <= key(2);
		if(key(2) = '0' AND key2prev = '1') then
			key2prev <= '0';
			if(playing = '0') theN
				triggerPlaying <= '1';
			end if;
		else
			if(playing = '1') theN
				triggerPlaying <= '0';
			end if;
		end if;
	
	end if;
	
end process;

	U1: VGA_SYNC_module PORT MAP
		(clock_50Mhz		=>	CLOCK_50,
		 red					=>	red_int,
		 green				=>	green_int,	
		 blue					=>	blue_int,
		 red_out				=>	VGA_R,
		 green_out			=>	VGA_G,
		 blue_out			=>	VGA_B,
		horiz_sync_out	=>	horiz_sync_int,
		 vert_sync_out		=>	vert_sync_int,
		 video_on			=>	VGA_BLANK_N,
		 pixel_clock		=>	VGA_CLK,
		 pixel_row			=>	pixel_row_int,
		 pixel_column		=>	pixel_column_int
		);

END structural;


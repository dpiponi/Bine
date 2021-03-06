{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- http://nesdev.com/6502_cpu.txt
-- http://www.zimmers.net/anonftp/pub/cbm/documents/chipdata/64doc

module Core(Emu6502(..), step, i8, i16, make16, irq, nmi, push, lo, hi) where
--module Core where

import Prelude hiding (and)
import Data.Word
import Control.Monad.State
import Data.Bits hiding (bit)
import Numeric

class (Monad m, MonadIO m) => Emu6502 m where
    readMemory :: Word16 -> m Word8
    writeMemory :: Word16 -> Word8 -> m ()
    getPC :: m Word16
    tick :: Int -> m ()
    putC :: Bool -> m ()
    getC :: m Bool
    putZ :: Bool -> m ()
    getZ :: m Bool
    putI :: Bool -> m ()
    getI :: m Bool
    putD :: Bool -> m ()
    getD :: m Bool
    putB :: Bool -> m ()
    getB :: m Bool
    putV :: Bool -> m ()
    getV :: m Bool
    putN :: Bool -> m ()
    getN :: m Bool
    getA :: m Word8
    putA :: Word8 -> m ()
    getS :: m Word8
    putS :: Word8 -> m ()
    getX :: m Word8
    putX :: Word8 -> m ()
    getP :: m Word8
    putP :: Word8 -> m ()
    getY :: m Word8
    putY :: Word8 -> m ()
    putPC :: Word16 -> m ()
    addPC :: Int -> m ()
    illegal :: Word8 -> m ()

    debugStr :: Int -> String -> m ()
    debugStrLn :: Int -> String -> m ()

{-# INLINABLE dumpRegisters #-}
dumpRegisters :: Emu6502 m => m ()
dumpRegisters = do
    -- XXX bring clock back
    --tClock <- use clock
    --debugStr 9 $ "clock = " ++ show tClock
    regPC <- getPC
    debugStr 9 $ " pc = " ++ showHex regPC ""
    regP <- getP
    debugStr 9 $ " flags = " ++ showHex regP ""
    debugStr 9 $ "(N=" ++ showHex ((regP `shift` (-7)) .&. 1) ""
    debugStr 9 $ ",V=" ++ showHex ((regP `shift` (-6)) .&. 1) ""
    debugStr 9 $ ",B=" ++ showHex (regP `shift` ((-4)) .&. 1) ""
    debugStr 9 $ ",D=" ++ showHex (regP `shift` ((-3)) .&. 1) ""
    debugStr 9 $ ",I=" ++ showHex (regP `shift` ((-2)) .&. 1) ""
    debugStr 9 $ ",Z=" ++ showHex (regP `shift` ((-1)) .&. 1) ""
    debugStr 9 $ ",C=" ++ showHex (regP .&. 1) ""
    regA <- getA 
    debugStr 9 $ ") A = " ++ showHex regA ""
    regX <- getX
    debugStr 9 $ " X = " ++ showHex regX ""
    regY <- getY
    debugStrLn 9 $ " Y = " ++ showHex regY ""
    regS <- getS
    debugStrLn 9 $ " N = " ++ showHex regS ""

{-# INLINABLE dumpMemory #-}
dumpMemory :: Emu6502 m => m ()
dumpMemory = do
    regPC <- getPC
    b0 <- readMemory regPC
    b1 <- readMemory (regPC+1)
    b2 <- readMemory (regPC+2)
    debugStr 9 $ "(PC) = "
    debugStr 9 $ showHex b0 "" ++ " "
    debugStr 9 $ showHex b1 "" ++ " "
    debugStrLn 9 $ showHex b2 ""

{-# INLINABLE dumpState #-}
dumpState :: Emu6502 m => m ()
dumpState = do
    dumpMemory
    dumpRegisters

{-# INLINE make16 #-}
make16 :: Word8 -> Word8 -> Word16
make16 lo0 hi0 = (i16 hi0 `shift` 8)+i16 lo0

{-# INLINE incPC #-}
incPC :: Emu6502 m => m ()
incPC = addPC 1

{-# INLINABLE read16 #-}
read16 :: Emu6502 m => Word16 -> m Word16
read16 addr = do
    lo0 <- readMemory addr
    hi0 <- readMemory (addr+1)
    return $ make16 lo0 hi0

{-# INLINABLE read16tick #-}
read16tick :: Emu6502 m => Word16 -> m Word16
read16tick addr = do
    tick 1
    lo0 <- readMemory addr
    tick 1
    hi0 <- readMemory (addr+1)
    return $ make16 lo0 hi0

{-# INLINABLE read16zpTick #-}
read16zpTick :: Emu6502 m => Word8 -> m Word16
read16zpTick addr = do
    tick 1
    lo0 <- readMemory (i16 addr)
    tick 1
    hi0 <- readMemory (i16 addr+1)
    return $ make16 lo0 hi0

-- http://www.emulator101.com/6502-addressing-modes.html

{-# INLINE i8 #-}
i8 :: Integral a => a -> Word8
i8 = fromIntegral

{-# INLINE i16 #-}
i16 :: Integral a => a -> Word16
i16 = fromIntegral

{-# INLINE iz #-}
iz :: Integral a => a -> Int
iz = fromIntegral

-- Note, a 6502 performs a read or write *every* clock cycle
-- regardless of what instruction is being executed.

-- 6 clock cycles...
{-# INLINABLE writeIndX #-}
writeIndX :: Emu6502 m => Word8 -> m ()
writeIndX src = do
    tick 1
    index <- getX
    addr <- getPC >>= readMemory

    tick 1
    discard $ readMemory (i16 addr)

    addrX <- read16zpTick (addr+index)

    tick 1
    writeMemory addrX src
    incPC

-- 3 clock cycles
{-# INLINABLE writeZeroPage #-}
writeZeroPage :: Emu6502 m => Word8 -> m ()
writeZeroPage src = do
    tick 1
    addr <- getPC >>= readMemory

    tick 1
    writeMemory (i16 addr) src
    incPC

-- 4 clock cycles
{-# INLINABLE writeAbs #-}
writeAbs :: Emu6502 m => Word8 -> m()
writeAbs src = do
    addr <- getPC >>= read16tick

    tick 1
    writeMemory addr src
    addPC 2

-- 6 clock cycles
{-# INLINABLE writeIndY #-}
writeIndY :: Emu6502 m => Word8 -> m ()
writeIndY src = do
    tick 1
    index <- getY
    addr' <- getPC >>= readMemory

    addr <- read16zpTick addr'

    let (halfAddrY, addrY) = halfSum addr index

    tick 1
    discard $ readMemory halfAddrY

    tick 1
    writeMemory addrY src
    incPC

-- 4 clock cycles
{-# INLINABLE writeZeroPageX #-}
writeZeroPageX :: Emu6502 m => Word8 -> m ()
writeZeroPageX src = do
    tick 1
    index <- getX
    addr <- getPC >>= readMemory

    tick 1
    discard $ readMemory (i16 addr)

    tick 1
    writeMemory (i16 $ addr+index) src
    incPC

-- 4 clock cycles
{-# INLINABLE writeZeroPageY #-}
writeZeroPageY :: Emu6502 m => Word8 -> m ()
writeZeroPageY src = do
    tick 1
    index <- getY
    addr <- getPC >>= readMemory

    tick 1
    discard $ readMemory (i16 addr)

    tick 1
    writeMemory (i16 $ addr+index) src
    incPC

-- 5 clock cycles
{-# INLINABLE writeAbsY #-}
writeAbsY :: Emu6502 m => Word8 -> m ()
writeAbsY src = do
    index <- getY
    addr <- getPC >>= read16tick

    tick 1
    let (halfAddrY, addrY) = halfSum addr index
    discard $ readMemory halfAddrY

    tick 1
    writeMemory addrY src
    addPC 2

-- 5 clock cycles
{-# INLINABLE writeAbsX #-}
writeAbsX :: Emu6502 m => Word8 -> m ()
writeAbsX src = do
    index <- getX
    addr <- getPC >>= read16tick

    tick 1
    let (halfAddrX, addrX) = halfSum addr index
    discard $ readMemory halfAddrX

    tick 1
    writeMemory addrX src
    addPC 2

-- 6 clock cycles
{-# INLINABLE readIndX #-}
readIndX :: Emu6502 m => m Word8
readIndX = do
    tick 1
    index <- getX
    addr0 <- getPC >>= readMemory

    tick 1
    discard $ readMemory (i16 addr0)

    addr1 <- read16zpTick (addr0+index)

    tick 1
    incPC
    readMemory addr1

-- 3 clock cycles
{-# INLINABLE readZeroPage #-}
readZeroPage :: Emu6502 m => m Word8
readZeroPage = do
    tick 1
    addr <- getPC >>= readMemory

    tick 1
    src <- readMemory (i16 addr)
    incPC
    return src

-- 2 clock cycles
{-# INLINABLE readImm #-}
readImm :: Emu6502 m => m Word8
readImm = do
    tick 1
    src <- getPC >>= readMemory
    incPC
    return src

-- XXX consider applicable ops like *>
-- 4 clock cycles
{-# INLINABLE readAbs #-}
readAbs :: Emu6502 m => m Word8
readAbs = do
    p0 <- getPC
    src <- (read16tick p0 <* tick 1) >>= readMemory
    addPC 2
    return src

-- 5-6 clock cycles
{-# INLINABLE readIndY #-}
readIndY :: Emu6502 m => m Word8
readIndY = do
    tick 1
    addr' <- getPC >>= readMemory

    addr <- read16zpTick addr'

    index <- getY
    let (halfAddrY, addrY) = halfSum addr index

    when (halfAddrY /= addrY) $ do
        tick 1
        discard $ readMemory halfAddrY

    tick 1
    src <- readMemory addrY
    incPC
    return src

-- 4 clock cycles
{-# INLINABLE readZeroPageX #-}
readZeroPageX :: Emu6502 m => m Word8
readZeroPageX = do
    tick 1
    index <- getX
    addr <- getPC >>= readMemory

    tick 1
    discard $ readMemory (i16 addr)

    tick 1
    incPC
    readMemory (i16 $ addr+index)

-- 4 clock cycles
{-# INLINABLE readZeroPageY #-}
readZeroPageY :: Emu6502 m => m Word8
readZeroPageY = do
    tick 1
    index <- getY
    addr <- getPC >>= readMemory

    tick 1
    discard $ readMemory (i16 addr)

    tick 1
    incPC
    readMemory (i16 $ addr+index)

{-# inline halfSum #-}
halfSum :: Word16 -> Word8 -> (Word16, Word16)
halfSum addr index = 
    let fullSum = addr+i16 index
    in (make16 (lo addr+index) (hi addr), fullSum)

{-# INLINABLE halfSignedSum #-}
halfSignedSum :: Word16 -> Word8 -> (Word16, Word16)
halfSignedSum addr index = 
    let fullSum = if index < 0x80 then addr+i16 index else addr+i16 index-0x100
    in (make16 (lo addr+index) (hi addr), fullSum)

-- 4-5 clock cycles
{-# INLINABLE readAbsX #-}
readAbsX :: Emu6502 m => m Word8
readAbsX = do
    index <- getX
    addr <- getPC >>= read16tick
    addPC 2

    let (halfAddrX, addrX) = halfSum addr index
    when (halfAddrX /= addrX) $ do
            tick 1
            discard $ readMemory halfAddrX

    tick 1
    readMemory addrX

-- 4-5 clock cycles
{-# INLINABLE readAbsY #-}
readAbsY :: Emu6502 m => m Word8
readAbsY = do
    index <- getY
    addr <- getPC >>= read16tick
    addPC 2

    let (halfAddrY, addrY) = halfSum addr index
    when ( halfAddrY /= addrY) $ do
            tick 1
            discard $ readMemory halfAddrY

    tick 1
    readMemory addrY

-- 2-4 clock cycles
{-# INLINABLE bra #-}
bra :: Emu6502 m => m Bool -> Bool -> m ()
bra getFlag value = do
    tick 1
    offset <- getPC >>= readMemory
    f <- getFlag
    incPC

    when (value == f) $ do
        tick 1
        discard $ getPC >>= readMemory

        oldP <- getPC
        let (halfAddr, addr) = halfSignedSum oldP offset
        when (halfAddr /= addr) $ do
                tick 1
                discard $ readMemory halfAddr
        putPC addr

-- 2 clock cycles
{-# INLINABLE set #-}
set :: Emu6502 m => (Bool -> m ()) -> Bool -> m ()
set putFlag value = do
    tick 1
    discard $ getPC >>= readMemory
    putFlag value

-- 2 clock cycles
{-# INLINABLE nop #-}
nop :: Emu6502 m => m ()
nop = do
    tick 1
    discard $ getPC >>= readMemory

{-
-- 3 clock cycles. Undocumented.
{-# INLINABLE nop #-}
dop :: Emu6502 m => m ()
nop = do
    tick 1
    discard $ getPC >>= readMemory
-}

-- 3 clock cycles
{-# INLINABLE jmp #-}
jmp :: Emu6502 m => m ()
jmp = getPC >>= read16tick >>= putPC

-- 5 clock cycles
-- NB address wraps around in page XXX
-- Not correct here.
-- Looks like the torture test might not catch this.
-- Aha! That's why ALIGN is used before addresses!
{-# INLINABLE jmp_indirect #-}
jmp_indirect :: Emu6502 m => m ()
jmp_indirect = do
    getPC >>= read16tick >>= read16tick >>= putPC

{-# INLINABLE uselessly #-}
uselessly :: m () -> m ()
uselessly = id

-- 5 clock cycles
{-# INLINABLE withZeroPage #-}
withZeroPage :: Emu6502 m => (Word8 -> m Word8) -> m ()
withZeroPage op = do
    tick 1
    addr <- getPC >>= readMemory

    tick 1
    src <- readMemory (i16 addr)

    tick 1
    uselessly $ writeMemory (i16 addr) src

    tick 1
    op src >>= writeMemory (i16 addr)
    incPC

-- 2 clock cycles
{-# INLINABLE withAcc #-}
withAcc :: Emu6502 m => (Word8 -> m Word8) -> m ()
withAcc op = do
    tick 1
    discard $ getPC >>= readMemory
    getA >>= op >>= putA

-- 6 clock cycles
{-# INLINE withAbs #-}
withAbs :: Emu6502 m => (Word8 -> m Word8) -> m ()
withAbs op = do
    addr <- getPC >>= read16tick
    
    tick 1
    src <- readMemory addr

    tick 1
    uselessly $ writeMemory addr src

    tick 1
    dst <- op src
    addPC 2
    writeMemory addr dst

-- 6 clock cycles
withZeroPageX :: Emu6502 m => (Word8 -> m Word8) -> m ()
withZeroPageX op = do
    tick 1
    index <- getX
    addr <- getPC >>= readMemory
    let addrX = addr+index

    tick 1
    discard $ readMemory (i16 addr)

    tick 1
    src <- readMemory (i16 addrX)

    tick 1
    writeMemory (i16 addrX) src

    tick 1
    dst <- op src
    writeMemory (i16 addrX) dst
    incPC
 
-- 7 clock cycles
{-# INLINE withAbsX #-}
withAbsX :: Emu6502 m => (Word8 -> m Word8) -> m ()
withAbsX op = do
    p0 <- getPC
    index <- getX
    addr <- read16tick p0

    let (halfAddrX, addrX) = halfSum addr index

    tick 1
    discard $ readMemory halfAddrX

    tick 1
    src <- readMemory addrX

    tick 1
    uselessly $ writeMemory addrX src

    tick 1
    addPC 2
    dst <- op src
    writeMemory addrX dst

{-# INLINABLE setN #-}
setN :: Emu6502 m => Word8 -> m ()
setN r = putN $ r >= 0x80

{-# INLINABLE setZ #-}
setZ :: Emu6502 m => Word8 -> m ()
setZ r = putZ $ r == 0

{-# INLINABLE setNZ #-}
setNZ :: Emu6502 m => Word8 -> m Word8
setNZ r = setN r >> setZ r >> return r

{-# INLINABLE setNZ_ #-}
setNZ_ :: Emu6502 m => Word8 -> m ()
setNZ_ r = setN r >> setZ r

{-# INLINABLE ora #-}
ora :: Emu6502 m => m Word8 -> m ()
ora mode = do
    src <- mode
    oldA <- getA
    let newA = oldA .|. src
    putA newA
    setNZ_ newA

{-# INLINABLE and #-}
and :: Emu6502 m => m Word8 -> m ()
and mode = do
    src <- mode
    getA >>= setNZ . (src .&.) >>= putA

{-# INLINABLE eor #-}
eor :: Emu6502 m => m Word8 -> m ()
eor mode = do
    src <- mode
    oldA <- getA
    let newA = oldA `xor` src
    putA newA
    void $ setNZ newA

{-# INLINABLE lda #-}
lda :: Emu6502 m => m Word8 -> m ()
lda mode = mode >>= setNZ >>= putA

{-# INLINABLE sta #-}
sta :: Emu6502 m => (Word8 -> m()) -> m ()
sta mode = getA >>= mode

{-# INLINABLE adc #-}
adc :: Emu6502 m => m Word8 -> m ()
adc mode = do
    src <- mode
    oldA <- getA
    carry <- getC
    let newA = fromIntegral oldA+fromIntegral src+if carry then 1 else 0 :: Word16
    decimal <- getD
    setZ (i8 newA)
    if decimal
        then do
            let adjustedA = if (oldA .&. 0xf) + (src .&. 0xf) + (if carry then 1 else 0) > 9
                                then newA+6
                                else newA
            setN (i8 adjustedA)
            putV $ (complement (fromIntegral oldA `xor` fromIntegral src) .&. 0x80) .&. ((fromIntegral oldA `xor` adjustedA) .&. 0x80) /= 0
            let readjustedA = if adjustedA > 0x99 then adjustedA+96 else adjustedA
            putC $ readjustedA > 0xff
            putA $ fromIntegral (readjustedA .&. 0xff)
        else do
            setN (i8 newA)
            putV $ (complement (fromIntegral oldA `xor` fromIntegral src) .&. 0x80) .&. ((fromIntegral oldA `xor` newA) .&. 0x80) /= 0
            putC $ newA > 0xff
            putA $ fromIntegral (newA .&. 0xff)

{-# INLINABLE sbc #-}
sbc :: Emu6502 m => m Word8 -> m ()
sbc mode = do
    src <- mode
    oldA <- getA
    carry <- getC
    let newA = fromIntegral oldA-fromIntegral src-if carry then 0 else 1 :: Word16
    discard $ setNZ $ i8 newA
    putV $ (((i16 oldA `xor` i16 src) .&. 0x80) /= 0) && (((i16 oldA `xor` newA) .&. 0x80) /= 0)
    decimal <- getD
    if decimal
        then do
            let adjustedA = if iz (oldA .&. 0xf)-(if carry then 0 else 1) < iz (src .&. 0xf)
                                then newA - 6
                                else newA
            let readjustedA = if adjustedA > 0x99
                                then adjustedA-0x60
                                else adjustedA
            putA $ fromIntegral (readjustedA .&. 0xff)
            putC $ readjustedA < 0x100
        else do
            putA $ fromIntegral (newA .&. 0xff)
            putC $ newA < 0x100

{-# INLINABLE cmp #-}
cmp :: Emu6502 m => m Word8 -> m ()
cmp mode = do
    src <- mode
    oldA <- getA
    let new = i16 oldA-i16 src
    putC $ new < 0x100
    setNZ_ $ i8 new

{-# INLINABLE asl #-}
asl :: Emu6502 m => ((Word8 -> m Word8) -> m ()) -> m ()
asl mode = mode $ \src -> do
    putC $ src .&. 0x80 > 0
    let new = src `shift` 1
    setNZ new

{-# INLINABLE rol #-}
rol :: Emu6502 m => ((Word8 -> m Word8) -> m ()) -> m ()
rol mode = mode $ \src -> do
    fc <- getC
    putC $ src .&. 0x80 > 0
    let new = (src `shift` 1) + if fc then 1 else 0
    setNZ new

{-# INLINABLE lsr #-}
lsr :: Emu6502 m => ((Word8 -> m Word8) -> m ()) -> m ()
lsr mode = mode $ \src -> do
    putC $ src .&. 0x01 > 0
    let new = src `shift` (-1)
    putN False
    setZ new
    return new

{-# INLINABLE ror #-}
ror :: Emu6502 m => ((Word8 -> m Word8) -> m ()) -> m ()
ror mode = mode $ \src -> do
    fc <- getC
    putC $ src .&. 0x01 > 0
    let new = (src `shift` (-1))+if fc then 0x80 else 0x00
    setNZ new

{-# INLINABLE stx #-}
stx :: Emu6502 m => (Word8 -> m()) -> m ()
stx mode = getX >>= mode

{-# INLINABLE ldx #-}
ldx :: Emu6502 m => m Word8 -> m ()
ldx mode = do
    src <- mode
    putX src
    setNZ_ src

{-# INLINABLE dec #-}
dec :: Emu6502 m => ((Word8 -> m Word8) -> m ()) -> m ()
dec mode = mode $ \src -> setNZ (src-1)

{-# INLINABLE inc #-}
inc :: Emu6502 m => ((Word8 -> m Word8) -> m ()) -> m ()
inc mode = mode $ \src -> setNZ (src+1)

{-# INLINABLE bit #-}
bit :: Emu6502 m => m Word8 -> m ()
bit mode = do
    src <- mode
    ra <- getA
    setN src
    putV $ src .&. 0x40 > 0
    setZ $ ra .&. src

{-# INLINABLE sty #-}
sty :: Emu6502 m => (Word8 -> m ()) -> m ()
sty mode = getY >>= mode

{-# INLINABLE ldy #-}
ldy :: Emu6502 m => m Word8 -> m ()
ldy mode = mode >>= setNZ >>= putY

{-# INLINABLE cpx #-}
cpx :: Emu6502 m => m Word8 -> m ()
cpx mode = do
    src <- mode
    rx <- getX
    let new = i16 rx-i16 src
    discard $ setNZ $ i8 new
    putC $ new < 0x100

{-# INLINABLE cpy #-}
cpy :: Emu6502 m => m Word8 -> m ()
cpy mode = do
    src <- mode
    ry <- getY
    let new = i16 ry-i16 src
    putC $ new < 0x100
    setNZ_ $ i8 new

-- 2 clock cycles
{-# INLINABLE txs #-}
txs :: Emu6502 m => m ()
txs = do
    tick 1
    discard $ getPC >>= readMemory
    getX >>= putS

-- 2 clock cycles
{-# INLINABLE tra #-}
tra :: Emu6502 m =>
                     m Word8 -> (Word8 -> m ()) ->
                     m ()
tra getReg putReg = do
    tick 1
    discard $ getPC >>= readMemory
    getReg >>= setNZ >>= putReg

-- 2 clock cycles
{-# INLINABLE inr #-}
inr :: Emu6502 m => m Word8 -> (Word8 -> m ()) -> m ()
inr getReg putReg = do
    tick 1
    discard $ getPC >>= readMemory
    v0 <- getReg
    let v1 = v0+1
    discard $ setNZ v1
    putReg v1

-- 2 clock cycles
{-# INLINABLE der #-}
der :: Emu6502 m => m Word8 -> (Word8 -> m ()) -> m ()
der getReg putReg = do
    tick 1
    discard $ getPC >>= readMemory
    v0 <- getReg
    let v1 = v0-1
    discard $ setNZ v1
    putReg v1

discard :: Emu6502 m => m Word8 -> m ()
discard = void

-- 7 clock cycles
{-# INLINABLE brk #-}
brk :: Emu6502 m => m ()
brk = do
    tick 1
    p0 <- getPC
    incPC
    discard $ readMemory p0

    p1 <- getPC
    incPC
    tick 1
    push $ hi p1

    incPC
    tick 1
    push $ lo p1

    putB True
    incPC
    tick 1
    getP >>= push . (.|. 0x20) -- always on bit
    putI True

    read16tick 0xfffe >>= putPC -- irq/brk XXX

-- Am I using wrong address for IRQ. Should it be 0xfffe for IRQ, 0xfffa for NMI?
-- XXX not supported correctly for now
{-# INLINABLE irq #-}
irq :: Emu6502 m => m ()
irq = do
    fi <- getI
    if not fi
        then nmi False
        else return ()

{-# INLINABLE push #-}
push :: Emu6502 m => Word8 -> m ()
push v = do
    sp <- getS
    writeMemory (0x100+i16 sp) v
    putS (sp-1)

{-# INLINABLE pull #-}
pull :: Emu6502 m => m Word8
pull = do
    sp <- getS
    let sp' = sp+1
    putS sp'
    readMemory (0x100+i16 sp')

-- 3 clock cycles
{-# INLINABLE pha #-}
pha ::Emu6502 m => m ()
pha = do
    tick 1
    discard $ getPC >>= readMemory

    tick 1
    getA >>= push

-- 3 clock cycles
{-# INLINABLE php #-}
php :: Emu6502 m => m ()
php = do
    tick 1
    discard $ getPC >>= readMemory

    tick 1
    getP >>= push . (.|. 0x30)

-- 4 clock cycles
{-# INLINABLE plp #-}
plp :: Emu6502 m => m ()
plp = do
    tick 1
    p0 <- getPC
    discard $ readMemory p0

    tick 1
    s <- getS
    discard $ readMemory (0x100+i16 s)

    tick 1
    pull >>= putP

-- 4 clock cycles
{-# INLINABLE pla #-}
pla :: Emu6502 m => m ()
pla = do
    tick 1
    p0 <- getPC
    discard $ readMemory p0

    tick 1
    s <- getS
    discard $ readMemory (0x100+i16 s)

    tick 1
    pull >>= setNZ >>= putA

{-# INLINABLE lo #-}
lo :: Word16 -> Word8
lo = i8

{-# INLINABLE hi #-}
hi :: Word16 -> Word8
hi a = i8 (a `shift` (-8))

{-# INLINABLE nmi #-}
nmi :: Emu6502 m => Bool -> m ()
nmi sw = do
    p0 <- getPC
    push $ hi p0
    push $ lo p0
    putB sw
    getP >>= push . (.|. 0x20) -- always on bit
    putI True
    read16 0xfffe >>= putPC -- irq/brk XXX
    tick 7

-- 6 clock cycles
{-# INLINABLE rti #-}
rti :: Emu6502 m => m ()
rti = do
    tick 1
    p0 <- getPC
    void $ readMemory p0

    tick 1
    s <- getS
    discard $ readMemory (0x100 + fromIntegral s)

    tick 1
    pull >>= putP

    make16 <$> (tick 1 >> pull) <*> (tick 1 >> pull) >>= putPC

-- 6 clock cycles
{-# INLINABLE jsr #-}
jsr :: Emu6502 m => m ()
jsr = do
    tick 1
    p0 <- getPC
    pcl <- readMemory p0
    incPC

    tick 1
    s <- getS
    discard $ readMemory (0x100 + fromIntegral s)

    p2 <- getPC

    tick 1
    push $ hi p2

    tick 1
    push $ lo p2

    tick 1
    pch <- readMemory p2

    putPC $ make16 pcl pch

-- 6 clock cycles
{-# INLINABLE rts #-}
rts :: Emu6502 m => m ()
rts = do
    tick 1
    discard $ getPC >>= readMemory

    tick 1
    s <- getS
    discard $ readMemory (0x100+i16 s)

    p0 <- make16 <$> (tick 1 >> pull) <*> (tick 1 >> pull)
    
    tick 1
    discard $ readMemory p0
    putPC (p0+1)

{-# INLINABLE step #-}
step :: Emu6502 m => m ()
step = do
    --dumpState
    p0 <- getPC
    tick 1
    i <- readMemory p0
    incPC
    case i of
        0x00 -> brk
        0x01 -> ora readIndX
        0x04 -> void $ readZeroPage -- XXX undocumented "DOP" nop
        0x05 -> ora readZeroPage
        0x06 -> asl withZeroPage
        0x08 -> php
        0x09 -> ora readImm
        0x0a -> asl withAcc
        0x0d -> ora readAbs
        0x0e -> asl withAbs
        0x10 -> bra getN False
        0x11 -> ora readIndY
        0x15 -> ora readZeroPageX
        0x16 -> asl withZeroPageX
        0x18 -> set putC False
        0x19 -> ora readAbsY
        0x1d -> ora readAbsX
        0x1e -> asl withAbsX
        0x20 -> jsr
        0x21 -> and readIndX
        0x24 -> bit readZeroPage
        0x25 -> and readZeroPage
        0x26 -> rol withZeroPage
        0x28 -> plp
        0x29 -> and readImm
        0x2a -> rol withAcc
        0x2c -> bit readAbs
        0x2d -> and readAbs
        0x2e -> rol withAbs
        0x30 -> bra getN True
        0x31 -> and readIndY
        0x35 -> and readZeroPageX
        0x36 -> rol withZeroPageX
        0x38 -> set putC True
        0x39 -> and readAbsY
        0x3d -> and readAbsX
        0x3e -> rol withAbsX
        0x40 -> rti
        0x41 -> eor readIndX
        0x45 -> eor readZeroPage
        0x46 -> lsr withZeroPage
        0x48 -> pha
        0x49 -> eor readImm
        0x4a -> lsr withAcc
        0x4c -> jmp
        0x4d -> eor readAbs
        0x4e -> lsr withAbs
        0x50 -> bra getV False
        0x51 -> eor readIndY
        0x55 -> eor readZeroPageX
        0x56 -> lsr withZeroPageX
        0x58 -> set putI False
        0x59 -> eor readAbsY
        0x5d -> eor readAbsX
        0x5e -> lsr withAbsX
        0x60 -> rts
        0x61 -> adc readIndX
        0x65 -> adc readZeroPage
        0x66 -> ror withZeroPage
        0x68 -> pla
        0x69 -> adc readImm
        0x6a -> ror withAcc
        0x6c -> jmp_indirect
        0x6d -> adc readAbs
        0x6e -> ror withAbs
        0x70 -> bra getV True
        0x71 -> adc readIndY
        0x75 -> adc readZeroPageX
        0x76 -> ror withZeroPageX
        0x78 -> set putI True
        0x79 -> adc readAbsY
        0x7d -> adc readAbsX
        0x7e -> ror withAbsX
        0x81 -> sta writeIndX
        0x84 -> sty writeZeroPage
        0x85 -> sta writeZeroPage
        0x86 -> stx writeZeroPage
        0x88 -> der getY putY
        0x8a -> tra getX putA
        0x8c -> sty writeAbs
        0x8d -> sta writeAbs
        0x8e -> stx writeAbs
        0x90 -> bra getC False
        0x91 -> sta writeIndY
        0x94 -> sty writeZeroPageX
        0x95 -> sta writeZeroPageX
        0x96 -> stx writeZeroPageY
        0x98 -> tra getY putA
        0x99 -> sta writeAbsY
        0x9a -> txs
        0x9d -> sta writeAbsX
        0xa0 -> ldy readImm
        0xa1 -> lda readIndX
        0xa2 -> ldx readImm
        0xa4 -> ldy readZeroPage
        0xa5 -> lda readZeroPage
        0xa6 -> ldx readZeroPage
        0xa8 -> tra getA putY
        0xa9 -> lda readImm
        0xaa -> tra getA putX
        0xac -> ldy readAbs
        0xad -> lda readAbs
        0xae -> ldx readAbs
        0xb0 -> bra getC True
        0xb1 -> lda readIndY
        0xb4 -> ldy readZeroPageX
        0xb5 -> lda readZeroPageX
        0xb6 -> ldx readZeroPageY
        0xb8 -> set putV False
        0xb9 -> lda readAbsY
        0xba -> tra getS putX
        0xbc -> ldy readAbsX
        0xbd -> lda readAbsX
        0xbe -> ldx readAbsY
        0xc0 -> cpy readImm
        0xc1 -> cmp readIndX
        0xc4 -> cpy readZeroPage
        0xc5 -> cmp readZeroPage
        0xc6 -> dec withZeroPage
        0xc8 -> inr getY putY
        0xc9 -> cmp readImm
        0xca -> der getX putX
        0xcc -> cpy readAbs
        0xcd -> cmp readAbs
        0xce -> dec withAbs
        0xd0 -> bra getZ False
        0xd1 -> cmp readIndY
        0xd5 -> cmp readZeroPageX
        0xd6 -> dec withZeroPageX
        0xd8 -> set putD False
        0xd9 -> cmp readAbsY
        0xdd -> cmp readAbsX
        0xde -> dec withAbsX
        0xe0 -> cpx readImm
        0xe1 -> sbc readIndX
        0xe4 -> cpx readZeroPage
        0xe5 -> sbc readZeroPage
        0xe6 -> inc withZeroPage
        0xe8 -> inr getX putX
        0xe9 -> sbc readImm
        0xea -> nop
        0xec -> cpx readAbs
        0xed -> sbc readAbs
        0xee -> inc withAbs
        0xf0 -> bra getZ True
        0xf1 -> sbc readIndY
        0xf5 -> sbc readZeroPageX
        0xf6 -> inc withZeroPageX
        0xf8 -> set putD True
        0xf9 -> sbc readAbsY
        0xfd -> sbc readAbsX
        0xfe -> inc withAbsX

        _ -> illegal i

    dumpState
    return ()

module CheckYourUnderstanding where

import qualified Control.Concurrent as CC
import qualified Control.Concurrent.MVar as MV
import qualified Data.ByteString.Char8 as B
import qualified Data.Locator as DL
import qualified Data.Time.Clock.POSIX as PSX
import qualified Filesystem as FS
import qualified Filesystem.Path.CurrentOS as FPC
import qualified Network.Info as NI
import qualified Safe
import Control.Exception (mask, try)
import Control.Monad (forever, when)
import Data.Bits
import Data.Bits.Bitwise (fromListBE)
import Data.List.Split (chunksOf)
import Database.Blacktip.Types
import System.IO.Unsafe (unsafePerformIO)

-- 1.
-- -> forever, when
-- 2.
-- -> Data.Bits, Database.Blacktip.Types
-- 3.
-- -> Types that are used for the database
-- 4.
-- a.
-- -> all three
-- b.
-- -> import qualified Filesystem
-- c.
-- -> import Control.Monad
